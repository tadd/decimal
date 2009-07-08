require "mkmf"

(cflags = arg_config("--cflags")) && $CFLAGS << " #{cflags}"
have_header "ruby/ruby.h"
have_func "rb_big_div"
have_func "rb_big_modulo"

create_makefile "decimal"
