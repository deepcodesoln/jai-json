#import "Basic";
#import "Compiler";

#run build();

build :: () {
  options := get_build_options();

  test := false;
  args := options.compile_time_command_line;
  for arg: args {
    if arg == {
      case "test";
        test = true;
      case;
        compiler_report(tprint("Unsupported argument '%'; supported options: 'test'\n", arg));
    }
  }

  if test {
    w := compiler_create_workspace();
    build_options := get_build_options();
    build_options.output_path = "build";
    build_options.intermediate_path = "build/intermediate";
    build_options.output_executable_name = "test_jai_json";
    set_optimization(*build_options, .DEBUG);
    set_build_options(build_options, w);

    add_build_file("test.jai", w);
  } else {
    w := compiler_create_workspace();
    build_options := get_build_options();
    build_options.output_path = "build";
    build_options.intermediate_path = "build/intermediate";
    build_options.output_executable_name = "jai_json";
    build_options.output_type = .DYNAMIC_LIBRARY;
    set_optimization(*build_options, .OPTIMIZED);
    set_build_options(build_options, w);

    add_build_file("module.jai", w);
  }

  set_build_options_dc(.{do_output=false});
}
