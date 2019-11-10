# strmtst

## to replicate:

```console
wget https://cs.fit.edu/~mmahoney/compression/enwik8.bz2
```

Edit the file path in `app/Main.hs` to point to the above file.

```console
$ stack build && stack exec strmtst
[...]
strmtst: unreachable state
CallStack (from HasCallStack):
  error, called at src/Streamly/Streams/StreamD.hs:3581:22 in streamly-0.6.1-JxXNzc4ZVeh50nx7ez5AYt:Streamly.Streams.StreamD
```
