# jai-json

A JSON deserializer for Jai programs.

## Building

You can build the distributable library with `jai build.jai`. You can build the
tests with `jai build.jai - test`. Output is written to `./build`.

## Usage

To use this in `other_project`

```
cd other_project/
mkdir modules
git submodule add git@github.com:deepcodesoln/jai-json.git modules/Json
```

Then, in `other_project/`

```
#import "Json";
```

## Known Issues:

1. No UTF-8 support, only ASCII.
1. No support for escaped strings in strings.
1. No validation of float schema; this will try to parse `1eee3` as a number.
1. No error handling; if we detect anything wrong, we terminate the program.
