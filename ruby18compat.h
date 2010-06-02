/*
 *  ruby18compat.h - write Ruby 1.8 compatible extention library
 *		     with 1.9 style
 *
 *  This program is under public domain.
 */

#ifndef RBIGNUM_SIGN
#define RBIGNUM_SIGN(x) RBIGNUM(x)->sign
#endif

#ifndef RBIGNUM_NEGATIVE_P
#define RBIGNUM_NEGATIVE_P(x) (!RBIGNUM_SIGN(x))
#endif

#ifndef RUBY_CRITICAL
#define RUBY_CRITICAL(statements) do {statements;} while (0)
#endif

#ifndef RB_NUM_COERCE_FUNCS_NEED_OPID
#define rb_num_coerce_bin(x, y, id) rb_num_coerce_bin(x, y)
#define rb_num_coerce_cmp(x, y, id) rb_num_coerce_cmp(x, y)
#define rb_num_coerce_relop(x, y, id) rb_num_coerce_relop(x, y)
#endif

#ifndef HAVE_RB_BIG_DIV
#define rb_big_div(x, y) (RARRAY_PTR(rb_big_divmod(x, y))[0])
#endif

#ifndef HAVE_RB_BIG_MODULO
#define rb_big_modulo(x, y) (RARRAY_PTR(rb_big_divmod(x, y))[1])
#endif
