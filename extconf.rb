require "mkmf"

cflags = arg_config("--cflags")
$CFLAGS << " #{cflags}" if cflags
version = if have_func('rb_gc_mark_threads')
            '193'
          else
            '200'
          end
$CFLAGS << " -DINUM_SOURCE_FILE=" + %(\\"inum#{version}.h\\")

create_makefile "decimal"
