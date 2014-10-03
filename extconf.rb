require "mkmf"

cflags = arg_config("--cflags")
$CFLAGS << " #{cflags}" if cflags
version = '193'
$CFLAGS << " -DINUM_SOURCE_FILE=" + %(\\"inum#{version}.h\\")

create_makefile "decimal"
