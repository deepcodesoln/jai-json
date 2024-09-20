#import "Basic";
#import "Hash_Table";

#import,file "module.jai";

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

test_auto_assign_object :: () {
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

test_auto_assign_array :: () {
  JSON :: #string DONE
[0, 1, 2, 3, 4]
  DONE

  a: [..] int;
  from_json(*a, JSON);
  for e, i: a {
    assert(e == i);
  }

  JSON_2 :: #string DONE
[[1]]
  DONE

  b: [..][..] int;
  from_json(*b, JSON_2);
  assert(b[0][0] == 1);
}

main :: () {
  test_parse_string();
  test_parse_number();
  test_parse_bool();
  test_parse_null();
  test_parse_object();
  test_parse_array();
  test_parse_value();
  test_auto_assign_object();
  test_auto_assign_array();
}
