use tokio::io::{self, Error, ErrorKind, AsyncWrite, AsyncRead, Write, Read};

use openssl::symm;


pub struct Encrypted<W> {
    inner: W,
    in_encr: Option<(symm::Crypter, )>,
    out_encr: Option<(symm::Crypter, Vec<u8>, usize)>,
}

impl<W: AsyncWrite + AsyncRead> Encrypted<W> {
    pub fn new(inner: W) -> Self {
        Encrypted { inner, in_encr: None, out_encr: None }
    }
    pub fn encrypt(&mut self, key: &[u8; 16]) -> Result<(),openssl::error::ErrorStack> {
        use symm::Mode::*;
        let c = |m| symm::Crypter::new(
            symm::Cipher::aes_128_cfb8(),
            m,
            key,
            Some(key)
        );
        self.in_encr = Some((c(Decrypt)?, ));
        self.out_encr = Some((c(Encrypt)?, Vec::new(), 0));
        Ok(())
    }
}

impl<W: AsyncWrite> AsyncWrite for Encrypted<W> {
    fn shutdown(&mut self) -> Result<futures::Async<()>,Error> {
        self.inner.shutdown()
    }
}
impl<W: AsyncWrite> Write for Encrypted<W> {
    fn write(&mut self, buf: &[u8]) -> Result<usize,Error> {
        if let Some((ref mut enc, ref mut inner_buf, ref mut offset)) = self.out_encr {
            if inner_buf.len() == 0 {
                // There's new data to be written. 16 is the block size of the cipher.
                inner_buf.resize(buf.len() + 16, 0);
                let n = enc.update(&buf, inner_buf)?;
                inner_buf.resize(n, 0);
                *offset = 0;
            }
            while inner_buf.len() > *offset {
                // Attempt to write into the underlying buffer until we either drain all of it or
                // receive a WouldBlock.
                *offset += self.inner.write(&inner_buf[*offset..])?;
            }
            inner_buf.clear();
            Ok(buf.len())
        } else {
            self.inner.write(buf)
        }
    }
    fn flush(&mut self) -> Result<(), Error> {
        self.inner.flush()
    }
}

