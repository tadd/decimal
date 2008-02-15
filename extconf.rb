require "mkmf"
(cflags = arg_config("--cflags")) && $CFLAGS << " #{cflags}"
create_makefile "decimal"
