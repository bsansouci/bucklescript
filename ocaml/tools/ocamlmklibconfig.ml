let bindir = "/Users/bsansouci/Desktop/bs-platform-native/native/4.06.1/bin"
let supports_shared_libraries = true
let default_rpath = ""
let mksharedlibrpath = ""
let toolpref = ""
let syslib x = "-l"^x;;
let mklib out files opts =      (* "" *)
  Printf.sprintf "ar rc %s %s %s; ranlib %s"
                 out opts files out;;
