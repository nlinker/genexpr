# How to profile haskell programs

It's recommended to thoroughly study RWH, chapter on profiling and optimization. This small project is intended to be a kick start.

## Running this

From the command line:
```
stack build --profile
stack exec -- test +RTS -p
```
