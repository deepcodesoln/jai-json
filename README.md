# jai-json

A JSON deserializer for Jai programs.

## Building

You can build the distributable library with `jai build.jai`. You can build the
tests with `jai build.jai - test`. Output is written to `./build`.

## Usage

To use this in `other_project`:

```
cd other_project/
mkdir modules
git submodule add git@github.com:deepcodesoln/jai-json.git modules/Json
```

Then, in `other_project/`:

```
Json :: #import "Json";
```

## API

See `module.jai` for full API documentation.

There are two general approaches to extracting data from parsed JSON.

The first is to extract data manually, element by element. This is done by
passing a JSON string to `parse_json` and then checking the `type` member of the
returned `*JSON_Value` to determine how to cast the value and interpret its
data.

The second is to provide a struct or array to `from_json` to parse JSON content
into. For restrictions and expectations on the passed struct or array, see the
full API documentation for `from_json`.

## Known Issues:

1. No UTF-8 support, only ASCII.
1. No support for escaped strings in strings.
1. No validation of float schema; this will try to parse `1eee3` as a number.
