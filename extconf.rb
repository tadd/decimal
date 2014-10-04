require "mkmf"

cflags = arg_config("--cflags")
$CFLAGS << " #{cflags}" if cflags
version = if have_func('rb_gc_mark_threads')
            '193'
          elsif have_func('rb_f_lambda')
            '200'
          else
            '21'
          end
$CFLAGS << " -DINUM_SOURCE_FILE=" + %(\\"inum#{version}.h\\")

create_makefile "decimal"
