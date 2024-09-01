# jai-json

A JSON deserializer for Jai programs. This only supports ASCII.

## Building

**Building**

```
jai first.jai
```

**Testing**

```
jai first.jai - test
```

Output is written to `./build`.

## Known Issues:

1. No support for escaped strings in strings.
1. No validation of float schema; this will try to parse `1eee3` as a number.
1. No error handling; if we detect anything wrong, we terminate the program.
