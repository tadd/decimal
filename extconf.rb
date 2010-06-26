require "mkmf"

(cflags = arg_config("--cflags")) && $CFLAGS << " #{cflags}"
have_header "ruby/ruby.h" # to distiguish Ruby 1.8 from 1.9 with <version.h>
have_func "rb_big_div"
have_func "rb_big_modulo"
have_func "rb_bigzero_p"

create_makefile "decimal"
