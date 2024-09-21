#import "Basic";
#import "Hash_Table";
#import "String";

// Populate `obj` from data parsed from `json`. `obj` must be a struct or a resizable
// array. For struct members and array elements, only `s64` (or `int`), `float64`, `bool`
// and `string` are supported. Structs and arrays can be nested. Arrays in the provided
// JSON are expected to contain only a single element type per array.
//
// This procedure does not support JSON `null`, and this procedure expects that if `obj`
// is a struct that all members in the struct have identically-named keys in the provided
// JSON. That is, this procedure does not support optional members or missing elements in
// the provided JSON.
from_json :: (obj: *$A, json: string) {
  json_value, _ := parse_value(json);
  auto_assign(cast(*u8)obj, type_info(A), json_value);
}

// Parse JSON content from a string. This returns the parsed content and any remaining
// characters from `s_` that were not processed.
parse_json :: parse_value;

// Populate the resizable array pointed to by `array` with the value provided in
// `json_value` guided by the `ti_array` describing the type semantics of what lives at
// `obj`.
auto_assign_array :: (array: *u8, ti_array: *Type_Info_Array, json_value: *JSON_Value) {
  assign :: (dst_array: *u8, json_array: *JSON_Array, _: $A, t_info: *Type_Info) {
    a: [..] A;
    array_resize(*a, json_array.v.count);
    for e, i: json_array.v {
      auto_assign(cast(*u8)*a[i], t_info, e);
    }
    memcpy(dst_array, cast(*u8)*a, size_of(type_of(a)));
  }

  json_array := cast(*JSON_Array)json_value;
  if ti_array.element_type.type == {
    case .INTEGER;
      assign(array, json_array, int, type_info(int));
    case .FLOAT;
      assign(array, json_array, float64, type_info(float64));
    case .BOOL;
      assign(array, json_array, bool, type_info(bool));
    case .STRING;
      assign(array, json_array, string, type_info(string));
    case .STRUCT;
      a: [..] u8;  // use byte array for type erasure
      array_reserve(*a, json_array.v.count * ti_array.element_type.runtime_size);
      a.count = json_array.v.count;
      for e, i: json_array.v {
        auto_assign(cast(*u8)(a.data + ti_array.element_type.runtime_size * i), ti_array.element_type, e);
      }
      memcpy(array, *a, size_of(type_of(a)));
    case .ARRAY;
      a: [..] u8;  // use byte array for type erasure
      array_reserve(*a, json_array.v.count * ti_array.element_type.runtime_size);
      a.count = json_array.v.count;
      for e, i: json_array.v {
        auto_assign_array(cast(*u8)(a.data + ti_array.element_type.runtime_size * i), cast(*Type_Info_Array)ti_array.element_type, e);
      }
      memcpy(array, *a, size_of(type_of(a)));
    case;
      assert(false, "unsupported auto_assign_array type '%'\n", ti_array.type);
  }
}

// Populate the memory pointed to by `obj` with the value provided in `json_value`
// guided by the `ti_obj` describing the type semantics of what lives at `obj`.
auto_assign :: (obj: *u8, ti_obj: *Type_Info, json_value: *JSON_Value) {
  if ti_obj.type == {
    case .INTEGER;
      assert(json_value.type == JSON_Number);
      i := cast(s64)((cast(*JSON_Number)json_value).v);
      assert(ti_obj.runtime_size == size_of(type_of(i)));
      memcpy(cast(*u8)obj, cast(*u8)*i, ti_obj.runtime_size);
    case .FLOAT;
      assert(json_value.type == JSON_Number);
      f := (cast(*JSON_Number)json_value).v;
      assert(ti_obj.runtime_size == size_of(type_of(f)));
      memcpy(cast(*u8)obj, cast(*u8)*f, ti_obj.runtime_size);
    case .BOOL;
      assert(json_value.type == JSON_Bool);
      b := (cast(*JSON_Bool)json_value).v;
      assert(ti_obj.runtime_size == size_of(type_of(b)));
      memcpy(cast(*u8)obj, cast(*u8)*b, ti_obj.runtime_size);
    case .STRING;
      assert(json_value.type == JSON_String);
      s := (cast(*JSON_String)json_value).v;
      assert(ti_obj.runtime_size == size_of(type_of(s)));
      memcpy(cast(*u8)obj, cast(*u8)*s, ti_obj.runtime_size);
    case .STRUCT;
      assert(json_value.type == JSON_Object);
      t := (cast(*JSON_Object)json_value).v;
      for member: (cast(*Type_Info_Struct)ti_obj).members {
        v, f := table_find(*t, member.name);
        assert(f);
        member_ptr := obj + member.offset_in_bytes;
        if member.type.type == {
          case .INTEGER;
            auto_assign(member_ptr, type_info(s64), v);
          case .FLOAT;
            auto_assign(member_ptr, type_info(float64), v);
          case .BOOL;
            auto_assign(member_ptr, type_info(bool), v);
          case .STRING;
            auto_assign(member_ptr, type_info(string), v);
          case .ARRAY;
            auto_assign_array(member_ptr, cast(*Type_Info_Array)member.type, v);
          case .STRUCT;
            auto_assign(member_ptr, member.type, v);
          case;
            assert(false, tprint("unsupported type '%'\n", member.type.type));
        }
      }
    case .ARRAY;
      assert(json_value.type == JSON_Array);
      auto_assign_array(obj, cast(*Type_Info_Array)ti_obj, json_value);
    case;
      assert(false, tprint("unsupported type '%'\n", ti_obj.type));
  }
}

// Parse a value out of `s_`. The first character of `s_` must be the first character
// of the value to parse. This returns a parsed value and a substring of `s_` starting
// at the first non-whitespace (see `eat_whitespace`) character after the substring
// containing the value in `s_`.
parse_value :: (s_: string) -> *JSON_Value, string {
  s := s_;
  value: *JSON_Value;

  if s[0] == {
    case #char "\"";
      value_string: string;
      value_string, s = parse_string(s);
      json_string: *JSON_String = alloc(size_of(JSON_String));
      json_string.type = JSON_String;
      json_string.v = value_string;
      value = json_string;
    case #char "t"; #through;
    case #char "f";
      value_bool: bool;
      value_bool, s = parse_bool(s);
      json_bool: *JSON_Bool = alloc(size_of(JSON_Bool));
      json_bool.type = JSON_Bool;
      json_bool.v = value_bool;
      value = json_bool;
    case #char "n";
      value_null: *void;
      value_null, s = parse_null(s);
      json_null: *JSON_Null = alloc(size_of(JSON_Null));
      json_null.type = JSON_Null;
      json_null.v = value_null;
      value = json_null;
    case #char "{";
      value_obj: Table(string, *JSON_Value);
      value_obj, s = parse_object(s);
      json_obj: *JSON_Object = alloc(size_of(JSON_Object));
      json_obj.type = JSON_Object;
      json_obj.v = value_obj;
      value = json_obj;
    case #char "[";
      value_arr: [..] *JSON_Value;
      value_arr, s = parse_array(s);
      json_arr: *JSON_Array = alloc(size_of(JSON_Array));
      json_arr.type = JSON_Array;
      json_arr.v = value_arr;
      value = json_arr;
    case;
      if is_digit(s[0]) || s[0] == #char "-" {
        value_num: float64;
        value_num, s = parse_number(s);
        json_num: *JSON_Number = alloc(size_of(JSON_Number));
        json_num.type = JSON_Number;
        json_num.v = value_num;
        value = json_num;
      } else {
        assert(false, "Unsupported value type.");
      }
  }

  return value, s;
}

// Parse an object out of `s_`. The first character must be `{`. This returns the object
// parsed out of `s_` and a substring of `s_` which begins with the next non-whitespace
// (see `eat_whitespace`) character after the substring containing the object in `s_`.
parse_object :: (s_: string) -> Table(string, *JSON_Value), string {
  s := s_;
  assert(s.count >= 2, "Not enough characters to parse an object.");
  assert(s[0] == #char "{", "The first character of object is not '['.");
  advance(*s, 1);

  obj: Table(string, *JSON_Value);

  while s {
    eat_whitespace(*s);
    if s[0] == #char "}" {
      advance(*s, 1);
      break;
    }

    // Find and parse key.
    key: string;
    key, s = parse_string(s);
    assert(!table_contains(*obj, key), "Duplicate key in table.");

    // Find and parse value.
    advance(*s, 1); // Eat ":".
    eat_whitespace(*s);
    value: *JSON_Value;
    value, s = parse_value(s);

    table_add(*obj, key, value);
    if s[0] == #char "," advance(*s, 1);
  }

  eat_whitespace(*s);
  return obj, s;
}

// Parse an array out of `s_`. The first character must be `[`. This returns the array
// parsed out of `s_` and a substring of `s_` which begins with the next non-whitespace
// (see `eat_whitespace`) character after the substring containing the array in `s_`.
parse_array :: (s_: string) -> [..] *JSON_Value, string {
  s := s_;
  assert(s.count >= 2, "Not enough characters to parse an array.");
  assert(s[0] == #char "[", "First character of array is not '['.");
  advance(*s, 1);

  arr: [..] *JSON_Value;

  while s {
    eat_whitespace(*s);
    if s[0] == #char "]" {
      advance(*s, 1);
      break;
    }

    value: *JSON_Value;
    value, s = parse_value(s);

    array_add(*arr, value);
    if s[0] == #char "," advance(*s, 1);
  }

  eat_whitespace(*s);
  return arr, s;
}

// Parse a string out of `s_`. The first character of `s_` is required to be `"`, and
// there must be a closing `"` elsewhere in the string. This returns the parsed string
// without opening and closing quotes and a substring of `s_` where the first character
// is the first non-whitespace (see `eat_whitespace`) character after the closing quote
// of the string parsed out of `s_`.
// @TODO(orphen) Handle escaped characters, particularly quotes.
parse_string :: (s_: string) -> string, string {
  s := s_;
  assert(s[0] == #char "\"", "Missing open quote for string value.");
  close_quote_idx := find_index_from_left(s, #char "\"", 1);
  assert(close_quote_idx != -1, "Missing closing quote for string value.");
  key := slice(s, 1, close_quote_idx - 1);
  advance(*s, close_quote_idx + 1);
  eat_whitespace(*s);
  return key, s;
}

// Parse a number out of `s_`. The first character must be a digit or `-`. This returns
// the parsed number. It also returns a substring of `s_` where the first character is
// the first non-whitespace (see `eat_whitespace`) character after the number parsed out
// of of `s_`.
// @TODO(orphen) Fix float identification (ex: don't allow for multiple `e` in floats).
// @TODO(orphen) Fix error checking (ex: no easy way to tell if `atof` succeeded).
parse_number :: (s_: string) -> float64, string {
  s := s_;
  assert(
    is_digit(s[0]) || s[0] == #char "-",
    "Wanted to parse a number, but first character is not a digit.");

  // Find where a substring that looks like a float ends.
  i := 1;  // Already checked 0 above.
  while i < s.count {
    if is_digit(s[i]) || s[i] == #char "." || s[i] == #char "e" || s[i] == #char "E" || s[i] == #char "+"|| s[i] == #char "-" {
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
  return fvalue, s;
}

// Parse a bool out of `s_`. This returns the parsed bool and a substring of `s_` where
// the first character is the first non-whitespace (see `eat_whitespace`) character after
// the `true` or `false` substring in `s_`.
parse_bool :: (s_: string) -> bool, string {
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
    assert(false, "When parsing a bool, literal was neither true nor false.");
  }
  eat_whitespace(*s);
  return value, s;
}

// Parse a null value out of `s_`. This returns `null` and a substring of `s_` where
// the first character is the first non-whitespace (see `eat_whitespace`) character
// after the `null` substring in `s_`.
parse_null :: (s_: string) -> *void, string {
  s := s_;
  n :: "null";
  assert(begins_with(s, n), "Expected `null` but found something else.");
  advance(*s, n.count);
  eat_whitespace(*s);
  return null, s;
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
