'use strict';

var Fs                      = require("fs");
var Path                    = require("path");
var Process                 = require("process");
var Child_process           = require("child_process");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

var delete_env_var = (
  function(process, key) { delete process.env[key] }
);

var map = {
  LIBDIR: "standard_library_default",
  BYTERUN: "standard_runtime",
  CCOMPTYPE: "ccomp_type",
  BYTECC: "bytecomp_c_compiler",
  BYTECCLIBS: "bytecomp_c_libraries",
  NATIVECC: "native_c_compiler",
  NATIVECCLIBS: "native_c_libraries",
  PACKLD: "native_pack_linker",
  RANLIBCMD: "ranlib",
  ARCMD: "ar",
  CC_PROFILE: "cc_profile",
  MKDLL: "mkdll",
  MKEXE: "mkexe",
  MKMAINDLL: "mkmaindll",
  ARCH: "architecture",
  MODEL: "model",
  SYSTEM: "system",
  ASM: "asm",
  ASM_CFI_SUPPORTED: "asm_cfi_supported",
  WITH_FRAME_POINTERS: "with_frame_pointers",
  EXT_OBJ: "ext_obj",
  EXT_ASM: "ext_asm",
  EXT_LIB: "ext_lib",
  EXT_DLL: "ext_lib",
  HOST: "host",
  TARGET: "target",
  SYSTHREAD_SUPPORT: "systhread_supported"
};

function gen_bsb_default_paths(jscomp_dir, bin_dir, ocaml_dir) {
  var bsb_default_paths = Path.join(jscomp_dir, "bsb", "bsb_default_paths.mlp");
  var content = Fs.readFileSync(bsb_default_paths, "utf8");
  var replace_values = function (_, match_, _$1, _$2) {
    switch (match_) {
      case "BIN_DIR" : 
          return JSON.stringify(bin_dir);
      case "OCAML_DIR" : 
          return JSON.stringify(ocaml_dir);
      default:
        return match_;
    }
  };
  var generated = content.replace((/\"%%(\w+)%%\"/g), replace_values);
  var bsb_default_paths_output = Path.join(jscomp_dir, "bsb", "bsb_default_paths.ml");
  Fs.writeFileSync(bsb_default_paths_output, generated, "utf8");
  return /* () */0;
}

function patch_config(jscomp_dir, config_map, is_windows) {
  var whole_compiler_config = Path.join(jscomp_dir, "bin", "config_whole_compiler.mlp");
  var whole_compiler_config_output = Path.join(jscomp_dir, "bin", "config_whole_compiler.ml");
  var content = Fs.readFileSync(whole_compiler_config, "utf8");
  var replace_values = function (_, match_, _$1, _$2) {
    if (match_ === "LIBDIR") {
      if (is_windows !== 0) {
        return "Filename.concat (Filename.concat (Filename.concat (Filename.dirname Sys.executable_name) \"..\") \"lib\") \"ocaml\"";
      } else {
        return JSON.stringify(Path.join(jscomp_dir, "..", "lib", "ocaml"));
      }
    } else {
      var match = map[match_];
      if (match !== undefined) {
        var match$1 = config_map[match];
        if (match$1 !== undefined) {
          return match$1;
        } else {
          console.log("No value found from ocamlopt.opt -config for \"" + (match + "\""));
          return "";
        }
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "config_compiler.ml",
                107,
                16
              ]
            ];
      }
    }
  };
  var generated = content.replace((/%%(\w+)%%/g), replace_values);
  Fs.writeFileSync(whole_compiler_config_output, generated, "utf8");
  return /* () */0;
}

function get_config_output(is_windows) {
  try {
    var ocamlc_config = is_windows ? "ocamlc.opt.exe -config" : "ocamlc.opt -config";
    var config_output = Child_process.execSync(ocamlc_config, {
          encoding: "utf8"
        });
    console.log("config_output:\n" + config_output);
    var keyvalues = config_output.split("\n").filter((function (x) {
              return +(x.length > 0);
            })).map((function (x) {
            var index = x.indexOf(":");
            var key = x.substr(0, index);
            var value = x.substr(index + 1 | 0);
            return /* tuple */[
                    key.trim(),
                    value.trim()
                  ];
          }));
    console.log("keyvalues");
    keyvalues.forEach((function (param) {
            console.log(param[0] + (": " + param[1]));
            return /* () */0;
          }));
    var accum_pairs = function (acc, param) {
      acc[param[0]] = param[1];
      return acc;
    };
    return /* Some */[keyvalues.reduce(accum_pairs, { })];
  }
  catch (exn){
    return /* None */0;
  }
}

function should_patch(config_map) {
  var match = config_map["version"];
  if (match !== undefined) {
    return +(match.indexOf("4.02.3") >= 0);
  } else {
    return /* false */0;
  }
}

var match = typeof (__dirname) === "undefined" ? undefined : (__dirname);

var dirname;

if (match !== undefined) {
  dirname = match;
} else {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "config_compiler.ml",
          147,
          14
        ]
      ];
}

var working_dir = Process.cwd();

console.log("Working dir " + working_dir);

delete_env_var(Process, "OCAMLPARAM");

Process.env["OCAMLRUNPARAM"] = "b";

var is_windows = +(Process.platform === "win32");

var main_bucklescript_dir = Path.join(dirname, "..");

var match$1 = Process.argv;

var match$2;

if (match$1.length !== 3) {
  match$2 = /* tuple */[
    Path.join(main_bucklescript_dir, "bin"),
    Path.join(main_bucklescript_dir, "vendor", "ocaml")
  ];
} else {
  var share = match$1[2];
  match$2 = /* tuple */[
    share,
    share
  ];
}

gen_bsb_default_paths(Path.join(main_bucklescript_dir, "jscomp"), match$2[0], match$2[1]);

var match$3 = get_config_output(is_windows);

if (match$3) {
  var config_map = match$3[0];
  if (should_patch(config_map)) {
    patch_config(Path.join(dirname, "..", "jscomp"), config_map, is_windows);
  } else {
    Process.exit(2);
  }
} else {
  console.log("System-installed OCaml compiler version not found");
  Process.exit(2);
}

/* delete_env_var Not a pure module */
