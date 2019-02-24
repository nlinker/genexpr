// from https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=4bc55825aa4f33b69e5f5b0d20db768a

#![feature(async_await, await_macro, futures_api)]
use std::error::Error;
use std::net::SocketAddr;
use futures::prelude::*;
use futures::executor;
use futures::channel::oneshot;
use futures::future::abortable;
use futures::task::SpawnExt;
use romio::tcp::{TcpStream};

async fn http_get(conn: &mut TcpStream) -> Result<String, Box<dyn Error + 'static>> {
    let _ = await!(conn.write_all(b"GET / HTTP/1.0\r\n\r\n"))?;
    let mut page = Vec::new();
    loop {
        let mut buf = vec![0; 128];
        let len = await!(conn.read(&mut buf))?;
        if len == 0 {
            break;
        }
        page.extend_from_slice(&buf[..len]);
    }
    let page = String::from_utf8_lossy(&page).into();
    Ok(page)
}

fn main() {
    executor::block_on(async {
        let mut pool = executor::ThreadPool::new().unwrap();
        let socket_addr = "74.125.232.233:80".parse().unwrap();
        let mut conn = await!(TcpStream::connect(&socket_addr)).unwrap();
        let (f, abort) = abortable(http_get(&mut conn));
        pool.spawn(async move {
            use std::thread;
            use std::time::Duration;
            thread::sleep(Duration::from_millis(10));
            abort.abort();
        });
        println!("{:?}", await!(f));
        println!("still has conn: {:?}", conn);
    });
}
