#import "Basic";
#import "Hash_Table";

#import,file "module.jai";

test_parse_string :: () {
  json :: "\"string\"   ";
  value, remainder, error := parse_string(json);
  assert(remainder.count == 0);
  assert(value == "string");
  assert(error == .NO_ERROR);
}

test_parse_string_failed :: () {
  json := "string\"";
  _, _, error := parse_string(json);
  assert(error == .INVALID_VALUE_FOR_JSON_STRING);

  json = "\"string";
  _, _, error = parse_string(json);
  assert(error == .INVALID_VALUE_FOR_JSON_STRING);
}

test_parse_number :: () {
  json := "123    ";
  value, remainder, error := parse_number(json);
  assert(remainder.count == 0);
  assert(value == 123);
  assert(error == .NO_ERROR);

  json = "123.4";
  value, _ = parse_number(json);
  assert(value == 123.4);

  json = "1e2";
  value, _ = parse_number(json);
  assert(value == 100);

  json = "1e-2";
  value, _ = parse_number(json);
  assert(value == 0.01);

  json = "-1";
  value, _ = parse_number(json);
  assert(value == -1);
}

test_parse_number_failed :: () {
  json :: "fail";
  value, remainder, error := parse_number(json);
  assert(error == .INVALID_VALUE_FOR_JSON_NUMBER);
}

test_parse_bool :: () {
  json := "true   ";
  value, remainder, error := parse_bool(json);
  assert(remainder.count == 0);
  assert(value == true);
  assert(error == .NO_ERROR);

  json = "false";
  value, _ = parse_bool(json);
  assert(value == false);
}

test_parse_bool_failed :: () {
  json :: "thanks";
  value, remainder, error := parse_bool(json);
  assert(error == .INVALID_VALUE_FOR_JSON_BOOL);
}

test_parse_null :: () {
  json :: "null   ";
  value, remainder, error := parse_null(json);
  assert(remainder.count == 0);
  assert(value == null);
  assert(error == .NO_ERROR);
}

test_parse_null_failed :: () {
  json :: "nope";
  value, remainder, error := parse_null(json);
  assert(error == .INVALID_VALUE_FOR_JSON_NULL);
}

test_parse_object :: () {
  json := "{ \"a\": \"b\" }   ";
  value, remainder, error := parse_object(json);
  assert(remainder.count == 0);
  assert(error == .NO_ERROR);
  json_v, f := table_find(*value, "a");
  assert(f);
  s := cast(*JSON_String) json_v;
  assert(s.type == JSON_String);
  assert(s.v == "b");

  // Test nested objects.
  json = "{ \"a\": { \"b\": \"c\" } }";
  value, _, error = parse_object(json);
  assert(error == .NO_ERROR);
  json_v, f = table_find(*value, "a");
  assert(f);
  o := cast(*JSON_Object) json_v;
  json_v, f = table_find(*o.v, "b");
  s = cast(*JSON_String) json_v;
  assert(f);
  assert(s.v == "c");
}

test_parse_object_failed :: () {
  json := "{";
  _, _, error := parse_object(json);
  assert(error == .INVALID_VALUE_FOR_JSON_OBJECT);

  json = "ab";
  _, _, error = parse_object(json);
  assert(error == .INVALID_VALUE_FOR_JSON_OBJECT);

  json = #string DONE
{
  "x": 1,
  "x": 2
}
  DONE
  _, _, error = parse_object(json);
  assert(error == .DUPLICATE_KEY);
}

test_parse_array :: () {
  json := "[ \"a\", \"b\" ]   ";
  value, remainder, error := parse_array(json);
  assert(remainder.count == 0);
  assert(value.count == 2);
  assert(error == .NO_ERROR);
  s := cast(*JSON_String) value[0];
  assert(s.type == JSON_String);
  assert(s.v == "a");

  // Test nested arrays.
  json = "[ \"a\", [ \"b\", \"c\" ] ]";
  value, _, error = parse_array(json);
  assert(value.count == 2);
  assert(error == .NO_ERROR);
  nested_array := cast(*JSON_Array) value[1];
  assert(nested_array.type == JSON_Array);
  assert(nested_array.v.count == 2);
  s = cast(*JSON_String) nested_array.v[1];
  assert(s.v == "c");
}

test_parse_array_failed :: () {
  json := "ab";
  _, _, error := parse_array(json);
  assert(error == .INVALID_VALUE_FOR_JSON_ARRAY);

  json = "[";
  _, _, error = parse_array(json);
  assert(error == .INVALID_VALUE_FOR_JSON_ARRAY);

  json = "[1, 2, 3";
  _, _, error = parse_array(json);
  assert(error == .INVALID_VALUE_FOR_JSON_ARRAY);
}

// This test just makes sure the correct code paths are taken and that structs are
// properly initialized.
test_parse_value :: () {
  json := "\"foo\"";
  value, remainder, error := parse_value(json);
  assert(remainder.count == 0);
  assert(error == .NO_ERROR);
  s := cast(*JSON_String) value;
  assert(s.type == JSON_String);
  assert(s.v == "foo");

  json = "1";
  value, _, error = parse_value(json);
  assert(error == .NO_ERROR);
  i := cast(*JSON_Number) value;
  assert(i.type == JSON_Number);
  assert(i.v == 1);

  json = "-1";
  value, _, error = parse_value(json);
  assert(error == .NO_ERROR);
  i = cast(*JSON_Number) value;
  assert(i.type == JSON_Number);
  assert(i.v == -1);

  json = "true";
  value, _, error = parse_value(json);
  assert(error == .NO_ERROR);
  b := cast(*JSON_Bool) value;
  assert(b.type == JSON_Bool);
  assert(b.v == true);

  json = "false";
  value, _, error = parse_value(json);
  assert(error == .NO_ERROR);
  b = cast(*JSON_Bool) value;
  assert(b.type == JSON_Bool);
  assert(b.v == false);

  json = "null";
  value, _, error = parse_value(json);
  assert(error == .NO_ERROR);
  n := cast(*JSON_Null) value;
  assert(n.type == JSON_Null);
  assert(n.v == null);

  json = "{}";
  value, _, error = parse_value(json);
  assert(error == .NO_ERROR);
  o := cast(*JSON_Object) value;
  assert(o.type == JSON_Object);
  assert(o.v.count == 0);

  json = "[]";
  value, _, error = parse_value(json);
  assert(error == .NO_ERROR);
  a := cast(*JSON_Array) value;
  assert(a.type == JSON_Array);
  assert(a.v.count == 0);
}

// This test just makes sure errors are propagated correctly.
test_parse_value_failed :: () {
  json := "\"foo";
  _, _, error := parse_value(json);
  assert(error == .INVALID_VALUE_FOR_JSON_STRING);

  // Unclear how to test for an invalid number with current logic.

  json = "truth";
  _, _, error = parse_value(json);
  assert(error == .INVALID_VALUE_FOR_JSON_BOOL);

  json = "nope";
  _, _, error = parse_value(json);
  assert(error == .INVALID_VALUE_FOR_JSON_NULL);

  json = "{";
  _, _, error = parse_value(json);
  assert(error == .INVALID_VALUE_FOR_JSON_OBJECT);

  json = "[";
  _, _, error = parse_value(json);
  assert(error == .INVALID_VALUE_FOR_JSON_ARRAY);

  json = "x";
  _, _, error = parse_value(json);
  assert(error == .UNKNOWN_JSON_VALUE_TYPE);
}

test_auto_assign_struct :: () {
  JSON :: #string DONE
{
  "a": 1,
  "b": -1.0,
  "c": false,
  "d": "foo bar baz",
  "e": [0, 1, 2, 3, 4],
  "f": [{"x": 0}, {"x": 1}],
  "g": {"x": 1}
}
  DONE
  
  Foo :: struct {
    a: int;
    b: float64;
    c: bool;
    d: string;
    e: [..] int;
    f: [..] Bar;
    g: Bar;
  }
  
  Bar :: struct {
    x: int;
  }

  f: Foo;
  from_json(*f, JSON);
  assert(f.a == 1);
  assert(f.b == -1.0);
  assert(f.c == false);
  assert(f.d == "foo bar baz");
  for e, i: f.e {
    assert(e == i);
  }
  for e, i: f.f {
    assert(e.x == i);
  }
  assert(f.g.x == 1);
}

test_auto_assign_struct_failed :: () {
  json :: "{\"a\": 1}";

  Foo :: struct {
    a: *int;
  }

  f: Foo;
  error := from_json(*f, json);
  assert(error == .UNSUPPORTED_ASSIGNMENT_TYPE);
}

test_auto_assign_array :: () {
  json := "[0, 1, 2, 3, 4]";

  a: [..] int;
  from_json(*a, json);
  for e, i: a {
    assert(e == i);
  }

  json = "[[1]]";

  b: [..][..] int;
  from_json(*b, json);
  assert(b[0][0] == 1);
}

test_auto_assign_array_failed :: () {
  json :: "[]";

  a: [..] *int;
  error := from_json(*a, json);
  assert(error == .UNSUPPORTED_ASSIGNMENT_TYPE);
}

test_auto_assign_failed :: () {
  // Int.
  i: u8 = 0;
  jv: JSON_Value = .{type = JSON_String};
  error := auto_assign(cast(*u8)*i, type_info(type_of(i)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_TYPES);

  jv = .{type = JSON_Number};
  error = auto_assign(cast(*u8)*i, type_info(type_of(i)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_RUNTIME_SIZES);

  // Float.
  jv = .{type = JSON_String};
  f: float32;
  error = auto_assign(cast(*u8)*f, type_info(type_of(f)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_TYPES);

  jv = .{type = JSON_Number};
  error = auto_assign(cast(*u8)*f, type_info(type_of(f)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_RUNTIME_SIZES);

  // Bool.
  b: bool;
  error = auto_assign(cast(*u8)*b, type_info(type_of(b)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_TYPES);

  // String.
  s: string;
  error = auto_assign(cast(*u8)*s, type_info(type_of(s)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_TYPES);

  // Struct.
  Foo :: struct {
    x: int;
  }
  foo: Foo;
  error = auto_assign(cast(*u8)*foo, type_info(type_of(foo)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_TYPES);

  obj: *JSON_Value;
  obj, _, error = parse_value("{}");
  assert(!error);
  error = auto_assign(cast(*u8)*foo, type_info(type_of(foo)), obj);
  assert(error == .NO_SUCH_JSON_KEY);

  // Array.
  ra: [..] int;
  error = auto_assign(cast(*u8)*ra, type_info(type_of(ra)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_TYPES);

  a: [1] int;
  error = auto_assign(cast(*u8)*a, type_info(type_of(a)), *jv);
  assert(error == .MISMATCHED_SOURCE_AND_DESTINATION_TYPES);
}

main :: () {
  test_parse_string();
  test_parse_string_failed();

  test_parse_number();
  test_parse_number_failed();

  test_parse_bool();
  test_parse_bool_failed();

  test_parse_null();
  test_parse_null_failed();

  test_parse_object();
  test_parse_object_failed();

  test_parse_array();
  test_parse_array_failed();

  test_parse_value();
  test_parse_value_failed();

  test_auto_assign_struct();
  test_auto_assign_struct_failed();

  test_auto_assign_array();
  test_auto_assign_array_failed();

  test_auto_assign_failed();
}
