/*
 *  ruby18compat.c - Ruby 1.8 compatible extention library with 1.9 style
 *
 *  This program is under public domain.
 */

#ifndef RBIGNUM_SIGN
#define RBIGNUM_SIGN(x) RBIGNUM(x)->sign
#endif

#ifndef RUBY_CRITICAL
#define RUBY_CRITICAL(statements) do {statements;} while (0)
#endif
