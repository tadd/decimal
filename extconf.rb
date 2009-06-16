require "mkmf"
(cflags = arg_config("--cflags")) && $CFLAGS << " #{cflags}"
have_header "ruby/ruby.h"

create_makefile "decimal"
