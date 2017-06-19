# How to profile haskell programs

It's recommended to thoroughly study RWH, chapter on profiling and optimization. This small project is intended to be a kick start.

## Default profiling

From the command line:
```
stack build --profile && stack exec -- teh-main +RTS -p
```
Then teh-main.hprof
```
COST CENTRE MODULE           %time %alloc

MAIN        MAIN               0.0   31.1
CAF         GHC.IO.Handle.FD   0.0   47.7
CAF         GHC.IO.Encoding    0.0    3.8
CAF         GHC.Event.Thread   0.0    1.7
main        Main               0.0   14.3
...
MAIN                        MAIN                     46           0    0.0   31.1     0.0  100.0
 main                       Main                     93           0    0.0   14.2     0.0   14.2
 CAF:main1                  Main                     90           0    0.0    0.0     0.0    0.1
...
CAF                        GHC.Event.Thread         84           0    0.0    1.7     0.0    1.7
CAF                        GHC.Conc.Signal          83           0    0.0    0.9     0.0    0.9
CAF                        GHC.IO.Encoding          82           0    0.0    3.8     0.0    3.8
CAF                        GHC.IO.Handle.FD         81           0    0.0   47.7     0.0   47.7
CAF                        GHC.IO.Handle.Text       80           0    0.0    0.1     0.0    0.1
CAF                        GHC.IO.Encoding.Iconv    61           0    0.0    0.3     0.0    0.3
```
