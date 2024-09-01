#load "jai_json.jai";

test_parse_string :: () {
  json :: "\"string\"   ";
  value, remainder := parse_string(json);
  assert(remainder.count == 0);
  assert(value == "string");
}

test_parse_number :: () {
  json := "123    ";
  value, remainder := parse_number(json);
  assert(remainder.count == 0);
  assert(value == 123);

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

test_parse_bool :: () {
  json := "true   ";
  value, remainder := parse_bool(json);
  assert(remainder.count == 0);
  assert(value == true);

  json = "false";
  value, _ = parse_bool(json);
  assert(value == false);
}

test_parse_null :: () {
  json :: "null   ";
  value, remainder := parse_null(json);
  assert(remainder.count == 0);
  assert(value == null);
}

test_parse_object :: () {
  json := "{ \"a\": \"b\" }   ";
  value, remainder := parse_object(json);
  assert(remainder.count == 0);
  json_v, f := table_find(*value, "a");
  assert(f);
  s := cast(*JSON_String) json_v;
  assert(s.type == JSON_String);
  assert(s.v == "b");

  // Test nested objects.
  json = "{ \"a\": { \"b\": \"c\" } }";
  value, _ = parse_object(json);
  json_v, f = table_find(*value, "a");
  assert(f);
  o := cast(*JSON_Object) json_v;
  json_v, f = table_find(*o.v, "b");
  s = cast(*JSON_String) json_v;
  assert(f);
  assert(s.v == "c");
}

test_parse_array :: () {
  json := "[ \"a\", \"b\" ]   ";
  value, remainder := parse_array(json);
  assert(remainder.count == 0);
  assert(value.count == 2);
  s := cast(*JSON_String) value[0];
  assert(s.type == JSON_String);
  assert(s.v == "a");

  // Test nested arrays.
  json = "[ \"a\", [ \"b\", \"c\" ] ]";
  value, _ = parse_array(json);
  assert(value.count == 2);
  nested_array := cast(*JSON_Array) value[1];
  assert(nested_array.type == JSON_Array);
  assert(nested_array.v.count == 2);
  s = cast(*JSON_String) nested_array.v[1];
  assert(s.v == "c");
}

// This test just makes sure the correct code paths are taken and that structs are
// properly initialized.
test_parse_value :: () {
  json := "\"foo\"";
  value, remainder := parse_value(json);
  assert(remainder.count == 0);
  s := cast(*JSON_String) value;
  assert(s.type == JSON_String);
  assert(s.v == "foo");

  json = "1";
  value, _ = parse_value(json);
  i := cast(*JSON_Number) value;
  assert(i.type == JSON_Number);
  assert(i.v == 1);

  json = "-1";
  value, _ = parse_value(json);
  i = cast(*JSON_Number) value;
  assert(i.type == JSON_Number);
  assert(i.v == -1);

  json = "true";
  value, _ = parse_value(json);
  b := cast(*JSON_Bool) value;
  assert(b.type == JSON_Bool);
  assert(b.v == true);

  json = "false";
  value, _ = parse_value(json);
  b = cast(*JSON_Bool) value;
  assert(b.type == JSON_Bool);
  assert(b.v == false);


  json = "null";
  value, _ = parse_value(json);
  n := cast(*JSON_Null) value;
  assert(n.type == JSON_Null);
  assert(n.v == null);

  json = "{}";
  value, _ = parse_value(json);
  o := cast(*JSON_Object) value;
  assert(o.type == JSON_Object);
  assert(o.v.count == 0);

  json = "[]";
  value, _ = parse_value(json);
  a := cast(*JSON_Array) value;
  assert(a.type == JSON_Array);
  assert(a.v.count == 0);
}

main :: () {
  test_parse_string();
  test_parse_number();
  test_parse_bool();
  test_parse_null();
  test_parse_object();
  test_parse_array();
  test_parse_value();
}
