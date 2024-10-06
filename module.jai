#import "Basic";
#import "Hash_Table";
#import "String";

Error :: enum u8 {
  NO_ERROR :: 0;
  INVALID_VALUE_FOR_JSON_NULL;
  INVALID_VALUE_FOR_JSON_BOOL;
  INVALID_VALUE_FOR_JSON_NUMBER;
  INVALID_VALUE_FOR_JSON_STRING;
  INVALID_VALUE_FOR_JSON_ARRAY;
  INVALID_VALUE_FOR_JSON_OBJECT;
  UNKNOWN_JSON_VALUE_TYPE;
  UNSUPPORTED_ASSIGNMENT_TYPE;
  DUPLICATE_KEY;
  MISMATCHED_SOURCE_AND_DESTINATION_TYPES;
  MISMATCHED_SOURCE_AND_DESTINATION_RUNTIME_SIZES;
  NO_SUCH_JSON_KEY;
}

// Populate `obj` from data parsed from `json`. `obj` must be a struct or a resizable
// array. For struct members and array elements, only `s64` (or `int`), `float64`, `bool`
// and `string` are supported. Structs and arrays can be nested. Arrays in the provided
// JSON are expected to contain only a single element type per array.
//
// This procedure does not support JSON `null`. If `allow_missing_keys` is false, this
// procedure expects that if `obj` is a struct that all members in the struct have
// identically-named keys in the provided JSON.
//
// If the returned error code is anything but `.NO_ERROR`, updates to `obj` should be
// treated as incomplete, and no member values should be relied on.
from_json :: (obj: *$A, json: string, allow_missing_keys: bool) -> Error {
  json_value, _, error := parse_value(json);
  if error return error;
  error = auto_assign(cast(*u8)obj, type_info(A), json_value, allow_missing_keys);
  return error;
}

// Parse JSON content from a string. This returns the parsed content, any remaining
// characters from `s_` that were not processed, and an error code. If the error code
// is anything but `.NO_ERROR`, parsing should be treated as failed.
parse_json :: parse_value;

// Populate the resizable array pointed to by `array` with the value provided in
// `json_value` guided by the `ti_array` describing the type semantics of what lives at
// `obj`.
//
// `allow_missing_keys` is not used directly but is passed through to recursive calls.
//
// If the return value is anything but `.NO_ERROR`, treat `array` as partially-assigned
// and thus invalid.
auto_assign_array :: (array: *u8, ti_array: *Type_Info_Array, json_value: *JSON_Value, allow_missing_keys: bool) -> Error {
  assign :: (dst_array: *u8, json_array: *JSON_Array, _: $A, t_info: *Type_Info, allow_missing_keys: bool) {
    a: [..] A;
    array_resize(*a, json_array.v.count);
    for e, i: json_array.v {
      auto_assign(cast(*u8)*a[i], t_info, e, allow_missing_keys);
    }
    memcpy(dst_array, cast(*u8)*a, size_of(type_of(a)));
  }

  json_array := cast(*JSON_Array)json_value;
  if ti_array.element_type.type == {
    case .INTEGER;
      assign(array, json_array, int, type_info(int), allow_missing_keys);
    case .FLOAT;
      assign(array, json_array, float64, type_info(float64), allow_missing_keys);
    case .BOOL;
      assign(array, json_array, bool, type_info(bool), allow_missing_keys);
    case .STRING;
      assign(array, json_array, string, type_info(string), allow_missing_keys);
    case .STRUCT;
      a: [..] u8;  // use byte array for type erasure
      array_reserve(*a, json_array.v.count * ti_array.element_type.runtime_size);
      a.count = json_array.v.count;
      for e, i: json_array.v {
        auto_assign(cast(*u8)(a.data + ti_array.element_type.runtime_size * i), ti_array.element_type, e, allow_missing_keys);
      }
      memcpy(array, *a, size_of(type_of(a)));
    case .ARRAY;
      a: [..] u8;  // use byte array for type erasure
      array_reserve(*a, json_array.v.count * ti_array.element_type.runtime_size);
      a.count = json_array.v.count;
      for e, i: json_array.v {
        error := auto_assign_array(cast(*u8)(a.data + ti_array.element_type.runtime_size * i), cast(*Type_Info_Array)ti_array.element_type, e, allow_missing_keys);
        if error then return error;
      }
      memcpy(array, *a, size_of(type_of(a)));
    case;
      log_error("When assigning an array, encountered unsupported element type %.", ti_array.type);
      return .UNSUPPORTED_ASSIGNMENT_TYPE;
  }
  return .NO_ERROR;
}

// Populate the memory pointed to by `obj` with the value provided in `json_value`
// guided by the `ti_obj` describing the type semantics of what lives at `obj`. If
// populating a struct, require a JSON object to have keys named identically to the
// struct members if `allow_missing_keys` is false.
//
// If the return value is anything but `.NO_ERROR`, treat `obj` as partially-assigned
// and thus invalid.
auto_assign :: (obj: *u8, ti_obj: *Type_Info, json_value: *JSON_Value, allow_missing_keys: bool) -> Error {
  if ti_obj.type == {
    case .INTEGER;
      if (json_value.type != JSON_Number) {
        log_error("When auto assigning, wanted a number but got %.", json_value.type);
        return .MISMATCHED_SOURCE_AND_DESTINATION_TYPES;
      }
      i := cast(s64)((cast(*JSON_Number)json_value).v);
      if (ti_obj.runtime_size != size_of(type_of(i))) {
        log_error("When auto assigning, found mismatch in source and destination sizes.");
        return .MISMATCHED_SOURCE_AND_DESTINATION_RUNTIME_SIZES;
      }
      memcpy(cast(*u8)obj, cast(*u8)*i, ti_obj.runtime_size);
    case .FLOAT;
      if (json_value.type != JSON_Number) {
        log_error("When auto assigning, wanted a number but got %.", json_value.type);
        return .MISMATCHED_SOURCE_AND_DESTINATION_TYPES;
      }
      f := (cast(*JSON_Number)json_value).v;
      if (ti_obj.runtime_size != size_of(type_of(f))) {
        log_error("When auto assigning, found mismatch in source and destination sizes.");
        return .MISMATCHED_SOURCE_AND_DESTINATION_RUNTIME_SIZES;
      }
      memcpy(cast(*u8)obj, cast(*u8)*f, ti_obj.runtime_size);
    case .BOOL;
      if (json_value.type != JSON_Bool) {
        log_error("When auto assigning, wanted a bool but got %.", json_value.type);
        return .MISMATCHED_SOURCE_AND_DESTINATION_TYPES;
      }
      b := (cast(*JSON_Bool)json_value).v;
      memcpy(cast(*u8)obj, cast(*u8)*b, ti_obj.runtime_size);
    case .STRING;
      if (json_value.type != JSON_String) {
        log_error("When auto assigning, wanted a string but got %.", json_value.type);
        return .MISMATCHED_SOURCE_AND_DESTINATION_TYPES;
      }
      s := (cast(*JSON_String)json_value).v;
      memcpy(cast(*u8)obj, cast(*u8)*s, ti_obj.runtime_size);
    case .STRUCT;
      if (json_value.type != JSON_Object) {
        log_error("When auto assigning, wanted an object but got %.", json_value.type);
        return .MISMATCHED_SOURCE_AND_DESTINATION_TYPES;
      }
      t := (cast(*JSON_Object)json_value).v;
      for member: (cast(*Type_Info_Struct)ti_obj).members {
        v, f := table_find(*t, member.name);
        if (!f) {
          log_error("When auto assigning, found no such key in JSON: %.", member.name);
          if (allow_missing_keys) {
            continue;
          } else {
            return .NO_SUCH_JSON_KEY;
          }
        }
        member_ptr := obj + member.offset_in_bytes;
        e: Error;
        if member.type.type == {
          case .INTEGER;
            e = auto_assign(member_ptr, type_info(s64), v, allow_missing_keys);
          case .FLOAT;
            e = auto_assign(member_ptr, type_info(float64), v, allow_missing_keys);
          case .BOOL;
            e = auto_assign(member_ptr, type_info(bool), v, allow_missing_keys);
          case .STRING;
            e = auto_assign(member_ptr, type_info(string), v, allow_missing_keys);
          case .ARRAY;
            e = auto_assign_array(member_ptr, cast(*Type_Info_Array)member.type, v, allow_missing_keys);
          case .STRUCT;
            e = auto_assign(member_ptr, member.type, v, allow_missing_keys);
          case;
            log_error("When assigning an object, encountered unsupported member type %.", member.type.type);
            e = .UNSUPPORTED_ASSIGNMENT_TYPE;
        }
        if e return e;
      }
    case .ARRAY;
      if (json_value.type != JSON_Array) {
        log_error("When auto assigning, wanted an array but got %.", json_value.type);
        return .MISMATCHED_SOURCE_AND_DESTINATION_TYPES;
      }
      ti_a := cast(*Type_Info_Array)ti_obj;
      if (ti_a.array_type != .RESIZABLE) {
        log_error("When auto assigning, got array, but it was not resizable.");
        return .MISMATCHED_SOURCE_AND_DESTINATION_TYPES;
      }
      e := auto_assign_array(obj, ti_a, json_value, allow_missing_keys);
      if e return e;
    case;
      log_error("When auto assigning, encountered unsupported type %.", ti_obj.type);
      return .UNSUPPORTED_ASSIGNMENT_TYPE;
  }
  return .NO_ERROR;
}

// Parse a value out of `s_`. The first character of `s_` must be the first character
// of the value to parse. This returns a parsed value, a substring of `s_` starting
// at the first non-whitespace (see `eat_whitespace`) character after the substring
// containing the value in `s_`, and an error code.
parse_value :: (s_: string) -> *JSON_Value, string, Error {
  s := s_;
  value: *JSON_Value;
  error := Error.NO_ERROR;

  if s[0] == {
    case #char "\"";
      value_string: string;
      value_string, s, error = parse_string(s);
      if error return null, s, error;
      json_string: *JSON_String = alloc(size_of(JSON_String));
      json_string.type = JSON_String;
      json_string.v = value_string;
      value = json_string;
    case #char "t"; #through;
    case #char "f";
      value_bool: bool;
      value_bool, s, error = parse_bool(s);
      if error return null, s, error;
      json_bool: *JSON_Bool = alloc(size_of(JSON_Bool));
      json_bool.type = JSON_Bool;
      json_bool.v = value_bool;
      value = json_bool;
    case #char "n";
      value_null: *void;
      value_null, s, error = parse_null(s);
      if error return null, s, error;
      json_null: *JSON_Null = alloc(size_of(JSON_Null));
      json_null.type = JSON_Null;
      json_null.v = value_null;
      value = json_null;
    case #char "{";
      value_obj: Table(string, *JSON_Value);
      value_obj, s, error = parse_object(s);
      if error return null, s, error;
      json_obj: *JSON_Object = alloc(size_of(JSON_Object));
      json_obj.type = JSON_Object;
      json_obj.v = value_obj;
      value = json_obj;
    case #char "[";
      value_arr: [..] *JSON_Value;
      value_arr, s, error = parse_array(s);
      if error return null, s, error;
      json_arr: *JSON_Array = alloc(size_of(JSON_Array));
      json_arr.type = JSON_Array;
      json_arr.v = value_arr;
      value = json_arr;
    case;
      if is_digit(s[0]) || s[0] == #char "-" {
        value_num: float64;
        value_num, s, error = parse_number(s);
        if error return null, s, error;
        json_num: *JSON_Number = alloc(size_of(JSON_Number));
        json_num.type = JSON_Number;
        json_num.v = value_num;
        value = json_num;
      } else {
        log_error("When parsing a value, cannot determine value type.");
        return null, s, .UNKNOWN_JSON_VALUE_TYPE;
      }
  }

  return value, s, error;
}

// Parse an object out of `s_`. The first character must be `{`. This returns the object
// parsed out of `s_`, a substring of `s_` which begins with the next non-whitespace
// (see `eat_whitespace`) character after the substring containing the object in `s_`,
// and an error code.
parse_object :: (s_: string) -> Table(string, *JSON_Value), string, Error {
  s := s_;

  obj: Table(string, *JSON_Value);
  if (s.count < 2) {
    log_error("When parsing object, not enough characters to encode an object.");
    return obj, s, .INVALID_VALUE_FOR_JSON_OBJECT;
  }
  if (s[0] != #char "{") {
    log_error("When parsing object, the first character was not '{'.");
    return obj, s, .INVALID_VALUE_FOR_JSON_OBJECT;
  }
  advance(*s, 1);

  while s {
    eat_whitespace(*s);
    if s[0] == #char "}" {
      advance(*s, 1);
      break;
    }

    // Find and parse key.
    key: string;
    key, s = parse_string(s);
    if (table_contains(*obj, key)) {
      log_error("When parsing object, found duplicate key %.", key);
      return obj, s, .DUPLICATE_KEY;
    }

    // Find and parse value.
    advance(*s, 1); // Eat ":".
    eat_whitespace(*s);
    value: *JSON_Value;
    value, s = parse_value(s);

    table_add(*obj, key, value);
    if s[0] == #char "," advance(*s, 1);
  }

  eat_whitespace(*s);
  return obj, s, .NO_ERROR;
}

// Parse an array out of `s_`. The first character must be `[`. This returns the array
// parsed out of `s_`,a substring of `s_` which begins with the next non-whitespace
// (see `eat_whitespace`) character after the substring containing the array in `s_`, and
// an error code.
parse_array :: (s_: string) -> [..] *JSON_Value, string, Error {
  s := s_;

  arr: [..] *JSON_Value;
  if !(s.count >= 2) {
    log_error("When parsing array, not enough characters to encode an array.");
    return arr, s, .INVALID_VALUE_FOR_JSON_ARRAY;
  }
  if (s[0] != #char "[") {
    log_error("When parsing array, the first character was not '['.");
    return arr, s, .INVALID_VALUE_FOR_JSON_ARRAY;
  }
  advance(*s, 1);

  found_end := false;
  while s {
    eat_whitespace(*s);
    if s[0] == #char "]" {
      advance(*s, 1);
      found_end = true;
      break;
    }

    value: *JSON_Value;
    value, s = parse_value(s);

    array_add(*arr, value);
    // @TODO(orphen) Fix parsing logic allowing no commas after non-terminal values.
    if s.count && s[0] == #char "," advance(*s, 1);
  }

  if (!found_end) {
    log_error("When parsing array, ran out of characters before finding `]`.");
    return arr, s, .INVALID_VALUE_FOR_JSON_ARRAY;
  }

  eat_whitespace(*s);
  return arr, s, .NO_ERROR;
}

// Parse a string out of `s_`. The first character of `s_` is required to be `"`, and
// there must be a closing `"` elsewhere in the string. This returns the parsed string
// without opening and closing quotes, a substring of `s_` where the first character is
// the first non-whitespace (see `eat_whitespace`) character after the closing quote
// of the string parsed out of `s_`, and an error code.
// @TODO(orphen) Handle escaped characters, particularly quotes.
parse_string :: (s_: string) -> string, string, Error {
  s := s_;
  if (s[0] != #char "\"") {
    log_error("When parsing string, missing open quote.");
    return "", s, .INVALID_VALUE_FOR_JSON_STRING;
  }
  close_quote_idx := find_index_from_left(s, #char "\"", 1);
  if (close_quote_idx == -1) {
    log_error("When parsing string, missing closing quote.");
    return "", s, .INVALID_VALUE_FOR_JSON_STRING;
  }
  key := slice(s, 1, close_quote_idx - 1);
  advance(*s, close_quote_idx + 1);
  eat_whitespace(*s);
  return key, s, .NO_ERROR;
}

// Parse a number out of `s_`. The first character must be a digit or `-`. This returns
// the parsed number, a substring of `s_` where the first character is the first
// non-whitespace (see `eat_whitespace`) character after the number parsed out out of
// `s_`, and an error code.
// @TODO(orphen) Fix float identification (ex: don't allow for multiple `e` in floats).
// @TODO(orphen) Fix error checking (ex: no easy way to tell if `atof` succeeded).
parse_number :: (s_: string) -> float64, string, Error {
  s := s_;
  if !(is_digit(s[0]) || s[0] == #char "-") {
    log_error("When parsing number, literal started with neither a digit nor `-` sign.");
    return 0.0, s, .INVALID_VALUE_FOR_JSON_NUMBER;
  }

  // Find where a substring that looks like a float ends.
  i := 1;  // Already checked 0 above.
  while i < s.count {
    if is_digit(s[i]) || s[i] == #char "." || s[i] == #char "e" || s[i] == #char "E" || s[i] == #char "+" || s[i] == #char "-" {
      i += 1;
    } else {
      break;
    }
  }

  num_str := slice(s, 0, i);
  cs := temp_c_string(slice(s, 0, i));
  fvalue := atof(cs);

  advance(*s, i);
  eat_whitespace(*s);
  return fvalue, s, .NO_ERROR;
}

// Parse a bool out of `s_`. This returns the parsed bool, a substring of `s_` where the
// first character is the first non-whitespace (see `eat_whitespace`) character after the
// `true` or `false` substring in `s_`, and an error code.
parse_bool :: (s_: string) -> bool, string, Error {
  s := s_;
  t :: "true";
  f :: "false";
  value: bool;
  if begins_with(s, t) {
    value = true;
    advance(*s, t.count);
  } else if begins_with(s, f) {
    value = false;
    advance(*s, f.count);
  } else {
    log_error("When parsing bool, literal was neither `true` nor `false`.");
    return value, s, .INVALID_VALUE_FOR_JSON_BOOL;
  }
  eat_whitespace(*s);
  return value, s, .NO_ERROR;
}

// Parse a null value out of `s_`. This returns `null`, a substring of `s_` where the
// first character is the first non-whitespace (see `eat_whitespace`) character after
// the `null` substring in `s_`, and an error code.
parse_null :: (s_: string) -> *void, string, Error {
  e: Error;
  s := s_;
  n :: "null";
  if (!begins_with(s, n)) {
    log_error("When parsing null, expected literal `null` but found something else.");
    return null, s, .INVALID_VALUE_FOR_JSON_NULL;
  }
  advance(*s, n.count);
  eat_whitespace(*s);
  return null, s, .NO_ERROR;
}

// A tag for creating discriminated unions in `JSON_*` types.
JSON_Value :: struct {
  type: Type;
}

JSON_Object :: struct {
  #as using base: JSON_Value;
  v: Table(string, *JSON_Value);
}

JSON_Array :: struct {
  #as using base: JSON_Value;
  v: [..] *JSON_Value;
}

JSON_String :: struct {
  #as using base: JSON_Value;
  v: string;
}

JSON_Number :: struct {
  #as using base: JSON_Value;
  v: float64;
}

JSON_Bool :: struct {
  #as using base: JSON_Value;
  v: bool;
}

JSON_Null :: struct {
  #as using base: JSON_Value;
  v : *void;
} 

// Advances the string pointed to by `s` to the next non-whitespace character or the end
// of the string if no such character exists.
eat_whitespace :: (s: *string) {
  SPACE           :: #char " ";
  HORIZONTAL_TAB  :: 9;
  LINE_FEED       :: 10;
  CARRIAGE_RETURN :: 13;

  while s.count {
    c := s.*[0];
    if c == SPACE || c == HORIZONTAL_TAB || c == LINE_FEED || c == CARRIAGE_RETURN {
      advance(s, 1);
    } else {
      break;
    }
  }
}
