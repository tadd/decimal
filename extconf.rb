require "mkmf"

(cflags = arg_config("--cflags")) && $CFLAGS << " #{cflags}"
version = if have_macro("RUBY_VERSION", "version.h")
            "18"
          elsif try_compile("int rb_str_hash(VALUE);")
            "191"
          else
            "192"
          end
$CFLAGS << " -DINUM_SOURCE_FILE=" + %(\\"inum#{version}.h\\")
have_func "rb_big_div"
have_func "rb_big_modulo"
have_func "rb_bigzero_p"
have_func "rb_usascii_str_new"

create_makefile "decimal"
