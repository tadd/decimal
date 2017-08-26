require "mkmf"

cflags = arg_config("--cflags")
$CFLAGS << " #{cflags}" if cflags
version = if have_func('rb_gc_mark_threads')
            '193'
          elsif have_func('rb_f_lambda')
            '200'
          elsif have_func('rb_fork')
            '21'
          elsif have_var('ruby_sourceline')
            have_func('rb_big_hash')
            '22'
          else
            have_func('rb_big_hash')
            '23'
          end
$CFLAGS << " -DINUM_SOURCE_FILE=" + %(\\"inum#{version}.h\\")

create_makefile "decimal"
