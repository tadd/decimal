/*
 *  decimal.c - implementation of Decimal,
 *              a multi-precision decimal arithmetic library
 *
 *  Copyright (C) 2003-2010 Tadashi Saito
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the Ruby License. See the file "COPYING" for
 *  more details.
 */

#include <ctype.h>
#include <float.h>
#define _ISOC99_SOURCE
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <ruby.h>
#ifdef HAVE_RUBY_UTIL_H
#include <ruby/util.h>
#else
#include <util.h>
#endif

/* we need support both of 1.8/1.9 with the same source! */
#include "ruby18compat.h"

/*
 * unfortunately, few copies of Integer functions
 * are needed from original Ruby
 */
#include INUM_SOURCE_FILE

/*
 * INUM_* macros: receive both Fixnum and Bignum,
 *                to operate any types of Integers transparently
 */
#define INUM_PLUS(a, b) \
    (FIXNUM_P(a) ? fix_plus(a, b) : rb_big_plus(a, b))
#define INUM_MINUS(a, b) \
    (FIXNUM_P(a) ? fix_minus(a, b) : rb_big_minus(a, b))
#define INUM_MUL(a, b) \
    (FIXNUM_P(a) ? fix_mul(a, b) : rb_big_mul(a, b))
#define INUM_DIV(a, b) \
    (FIXNUM_P(a) ? fix_div(a, b) : rb_big_div(a, b))
#define INUM_DIVMOD(a, b) \
    (FIXNUM_P(a) ? fix_divmod(a, b) : rb_big_divmod(a, b))
#define INUM_POW(a, b) \
    (FIXNUM_P(a) ? fix_pow(a, b) : rb_big_pow(a, b))
#define INUM_EQ(a, b) \
    (FIXNUM_P(a) ? fix_equal(a, b) : rb_big_eq(a, b))
#define INUM_CMP(a, b) \
    (FIXNUM_P(a) ? fix_cmp(a, b) : rb_big_cmp(a, b))
#define INUM_GT(a, b) (FIX2INT(INUM_CMP(a, b)) > 0)
#define INUM_UMINUS(n) \
    (FIXNUM_P(n) ? LONG2NUM(-FIX2LONG(n)) : rb_big_uminus(n))
#define INUM_HASH(n) \
    (FIXNUM_P(n) ? rb_obj_id(n) : rb_big_hash(n))
#define INUM2STR(n) \
    (FIXNUM_P(n) ? rb_fix2str(n, 10) : rb_big2str(n, 10))
#define INUM_ODD_P(n) \
    (FIXNUM_P(n) ? fix_odd_p(n) : rb_big_odd_p(n))

/* implementation-independent INUM_* macros */
#define INUM_INC(n) do { n = INUM_PLUS(n, INT2FIX(1)); } while (0)
#define INUM_DEC(n) do { n = INUM_MINUS(n, INT2FIX(1)); } while (0)
#define INUM_ZERO_P(n) (FIXNUM_P(n) && FIX2LONG(n) == 0)
#define INUM_NEGATIVE_P(n) (FIXNUM_P(n) ? FIX2LONG(n) < 0 : RBIGNUM_NEGATIVE_P(n))
#define INUM_BOTTOMDIG(n) (FIXNUM_P(n) ? FIX2LONG(n) % 10 : \
    !rb_bigzero_p(n) ? FIX2INT(rb_big_modulo(n, INT2FIX(10))) : 0)

/* the body */
typedef struct {
    VALUE inum;
    long scale;
} Decimal;

/* singleton objects for NaN, +Infinity, -Infinity */
static Decimal *const DEC_NaN = (Decimal *)1;
static Decimal *const DEC_PINF = (Decimal *)3;
static Decimal *const DEC_NINF = (Decimal *)7;
/* and their representation as Ruby objects */
static VALUE VALUE_NaN, VALUE_PINF, VALUE_NINF;

#define CHECK_NAN_WITH_VAL(v, retval) do { \
    if (v == VALUE_NaN) return retval; } while (0)
#define CHECK_NAN2_WITH_VAL(v1, v2, retval) do { \
    if (v1 == VALUE_NaN || v2 == VALUE_NaN) return retval; } while (0)
#define CHECK_NAN(v) CHECK_NAN_WITH_VAL(v, VALUE_NaN)
#define CHECK_NAN2(v1, v2) CHECK_NAN2_WITH_VAL(v1, v2, VALUE_NaN)

/* special constants - i.e. non-zero and non-fixnum */
/* used for signed zeros that never meet any fixnums nor normal VALUEs */
static const VALUE DEC_PZERO = 2, DEC_NZERO = 6;
#define dec_pzero(scale) WrapDecimal(dec_raw_new(DEC_PZERO, scale))
#define dec_nzero(scale) WrapDecimal(dec_raw_new(DEC_NZERO, scale))

#define DEC_ISINF(d) ((d) == DEC_PINF || (d) == DEC_NINF)
#define DEC_VALUE_ISINF(v) ((v) == VALUE_PINF || (v) == VALUE_NINF)
/* immediate means non-finite */
#define DEC_IMMEDIATE_P(d) (DEC_ISINF(d) || (d) == DEC_NaN)
/* special signed zeros */
#define INUM_SPZERO_P(n) ((n) == DEC_PZERO || (n) == DEC_NZERO)
#define DEC_ZERO_P(d) INUM_SPZERO_P((d)->inum)

/* use internally in to_f */
static Decimal *DEC_DBL_MIN = NULL, *DEC_DBL_MAX = NULL;
static VALUE INUM_DBL_MAX = Qnil;
#define GET_DEC_DBL_MAX() (DEC_DBL_MAX != NULL ? DEC_DBL_MAX : \
			   (DEC_DBL_MAX = dbl_threshold_to_dec(DBL_MAX)))
#define GET_DEC_DBL_MIN() (DEC_DBL_MIN != NULL ? DEC_DBL_MIN : \
			   (DEC_DBL_MIN = dbl_threshold_to_dec(DBL_MIN)))
#define GET_INUM_DBL_MAX() (INUM_DBL_MAX != Qnil ? INUM_DBL_MAX : \
			    dbl_threshold_to_inum(DBL_MAX, &INUM_DBL_MAX))

/*
 * all rounding modes
 */
static VALUE ROUND_CEILING;
static VALUE ROUND_DOWN;
static VALUE ROUND_FLOOR;
static VALUE ROUND_HALF_DOWN;
static VALUE ROUND_HALF_EVEN;
static VALUE ROUND_HALF_UP;
static VALUE ROUND_UP;
static VALUE ROUND_UNNECESSARY;

#define GetDecimal(obj, d) do { \
    Data_Get_Struct(obj, Decimal, d); \
    if (d == NULL) rb_raise(rb_eArgError, "uninitialized Decimal object"); \
} while (0)
#define WrapDecimal(d) Data_Wrap_Struct(cDecimal, dec_mark, dec_free, d)
#define WrapStatic(d) Data_Wrap_Struct(cDecimal, NULL, NULL, d)
#define DECIMAL_P(d) (CLASS_OF(d) == cDecimal)

static VALUE cDecimal;
static VALUE eDomainError;
static VALUE eArithmeticError;

/* mark if d->inum is a Bignum */
static void
dec_mark(void *p)
{
    const Decimal *const d = p;

    if (d == NULL) return; /* uninitialized object */
    if (!FIXNUM_P(d->inum) && !INUM_SPZERO_P(d->inum)) {
        rb_gc_mark(d->inum); /* mark a Bignum */
    }
}

#ifndef DEBUG
#define dec_free (-1) /* just xfree() the objects */
#else
static void
dec_free(void *p)
{
    static void *prev = NULL;

    if (p == prev) {
        fprintf(stderr, "dec_free(): double-free occurred on %p\n", p);
    }
    xfree(p);
    prev = p;
}
#endif

static inline Decimal *
dec_raw_new(VALUE inum, long scale)
{
    Decimal *d = ALLOC(Decimal);

    d->inum = inum;
    d->scale = scale;
    return d;
}

static Decimal *
inum_to_dec(VALUE x)
{
    return dec_raw_new(INUM_ZERO_P(x) ? DEC_PZERO : x, 0);
}

static VALUE
cstr_to_inum(VALUE str)
{
    return rb_cstr_to_inum((const char *)str, 10, Qtrue);
}

static VALUE
invalid_str(VALUE arg)
{
    VALUE *assoc = (VALUE *)arg;

    xfree((char *)assoc[0]);
    rb_invalid_str((const char *)assoc[1], "Decimal");
    return Qnil; /* not reached */
}

static Decimal *
cstr_to_dec(const char *str)
{
    char *const s = strdup(str);
    char *ss;
    long scale = 0;
    VALUE inum, assoc[2];

    assoc[0] = (VALUE)s;
    assoc[1] = (VALUE)str;
    if (ss = strpbrk(s, "Ee")) {
        *ss++ = '\0'; /* for strchr() */
        inum = rb_rescue(cstr_to_inum, (VALUE)ss, invalid_str, (VALUE)assoc);
        scale -= NUM2LONG(inum);
    }
    if (ss = strchr(s, '.')) {
        const char *p;

        *ss = '_'; /* so that rb_cstr_to_inum() can ignore '.' */
        for (p = ss + 1; ISDIGIT(*p) || *p == '_'; p++) {
            if (ISDIGIT(*p)) scale++;
        }
    }
    inum = rb_rescue(cstr_to_inum, (VALUE)s, invalid_str, (VALUE)assoc);
    if (INUM_ZERO_P(inum)) {
        inum = strchr(s, '-') ? DEC_NZERO : DEC_PZERO;
    }
    xfree(s);
    return dec_raw_new(inum, scale);
}

static Decimal *
finite_dup(Decimal *d)
{
    VALUE inum;

    if (FIXNUM_P(d->inum) || INUM_SPZERO_P(d->inum)) {
        inum = d->inum;
    }
    else {
        inum = rb_big_clone(d->inum); /* inum is a Bignum */
    }
    return dec_raw_new(inum, d->scale);
}

static Decimal *
create_dec(VALUE arg)
{
    switch (TYPE(arg)) {
      case T_FIXNUM:
      case T_BIGNUM:
        return inum_to_dec(arg);
      case T_STRING:
        return cstr_to_dec(StringValueCStr(arg));
      case T_FLOAT:
        rb_raise(rb_eArgError, "invalid type Float: %s",
                 RSTRING_PTR(rb_inspect(arg)));
      default:
        rb_raise(rb_eArgError, "invalid value for Decimal: %s",
                 RSTRING_PTR(rb_inspect(arg)));
    }
    return NULL; /* not reached */
}

/* TODO: should know about allocation framework for dumping/loading */
static VALUE
dec_s_allocate(VALUE klass)
{
    return Data_Wrap_Struct(klass, dec_mark, dec_free, NULL);
}

/*
 *  call-seq:
 *     Decimal.new(arg)   => decimal
 *
 *  Returns a new decimal made from _arg_.  The _arg_ must be an +Integer+
 *  or a +String+.  An acceptable format of +String+ is equal to
 *  <code>Kernel.Float()</code>'s one.  In a +Regexp+, it should be:
 *
 *     digits  = /(\d+_)*\d+/
 *     number  = /(\+-)?#{digits}/
 *     body    = /#{number}(\.#{digits})?([eE]#{number})?/
 *     decimal = /\A\s*#{body}\s*\z/
 *
 *  And its samples are:
 *
 *     Decimal(1)                  #=> Decimal(1)
 *     Decimal(2**64)              #=> Decimal(18446744073709551616)
 *     Decimal("1")                #=> Decimal(1)
 *     Decimal("1.1")              #=> Decimal(1.1)
 *     Decimal("1e10")             #=> Decimal(10000000000)
 *     Decimal("299_792_458")      #=> Decimal(299792458)
 *     Decimal("2.99_792_458e8")   #=> Decimal(299792458)
 *
 *  Notice that a +Float+ is *not* acceptable for _arg_ to keep exactness.
 *
 *     Decimal.new(1.1)            #=> (ArgumentError)
 */
static VALUE
dec_initialize(VALUE self, VALUE arg)
{
    if (DECIMAL_P(arg)) {
	return arg;
    }
    DATA_PTR(self) = create_dec(arg);
    return self;
}

/*
 *  call-seq:
 *     Decimal(arg)   => decimal
 *
 *  Identical to <code>Decimal.new(arg)</code>, except that this method
 *  never be affected from overriding <code>Decimal#initialize</code>.
 */
static VALUE
f_decimal(VALUE klass_unused, VALUE arg)
{
    return dec_initialize(dec_s_allocate(cDecimal), arg);
}

#ifdef DEBUG
/* :nodoc: */
static VALUE
dec_scale(VALUE self)
{
    Decimal *d;

    GetDecimal(self, d);
    if (DEC_IMMEDIATE_P(d)) return Qnil;
    return LONG2NUM(d->scale);
}

/* :nodoc: */
static VALUE
dec_unscaled_value(VALUE self)
{
    Decimal *d;

    GetDecimal(self, d);
    if (DEC_IMMEDIATE_P(d)) return Qnil;
    return DEC_ZERO_P(d) ? INT2FIX(0) : d->inum;
}

/* :nodoc: */
static VALUE
dec_strip_trailing_zeros(VALUE self)
{
    Decimal *d, *d2;

    GetDecimal(self, d);
    if (DEC_IMMEDIATE_P(d))
	return self;
    if (DEC_ZERO_P(d)) { /* XXX: negative scale? */
        if (d->scale <= 0) return self;
	d2 = finite_dup(d);
	d2->scale = 0;
	return WrapDecimal(d2);
    }
    d2 = finite_dup(d);
    /* TODO: can be optimized with dividing each part
     * for Bignums and Fixnums */
    while (INUM_BOTTOMDIG(d2->inum) == 0) {
	d2->inum = INUM_DIV(d2->inum, INT2FIX(10));
	d2->scale--;
    }
    return WrapDecimal(d2);
}
#endif /* DEBUG */

/* FIXME: should return "%g" format string */
static VALUE
finite_to_s(Decimal *d)
{
    const VALUE str = INUM2STR(d->inum);
    const char *s = RSTRING_PTR(str);
    const long slen = RSTRING_LEN(str);
    const long scale = d->scale;
    long snumlen, sslen, diff;
    int negative;
    char *ss; /* source of newstr */
    VALUE newstr; /* to be returned */

    if (scale == 0) return str;
    if (scale < 0) { /* "xx00" */
	sslen = slen - scale;
	ss = ALLOC_N(char, sslen);
	memcpy(ss, s, slen);
	memset(ss+slen, '0', -scale);
	goto coda;
    }
    negative = (*s == '-');
    snumlen = negative ? slen-1 : slen;
    if (scale < snumlen) { /* "xx.xx" */
	diff = slen - scale;
	sslen = slen + 1;
	ss = ALLOC_N(char, sslen);
	memcpy(ss, s, diff);
	ss[diff] = '.';
	memcpy(ss+diff+1, s+diff, scale);
    }
    else { /* "0.00xx" */
	char *ss2; /* alias of ss */

	diff = scale - snumlen;
	/* "0." + "00..." + "-?xx" */
	sslen = 2 + diff + slen;
	ss = ss2 = ALLOC_N(char, sslen);
	if (negative) *ss2++ = '-', s++;
	memcpy(ss2, "0.", 2);
	ss2 += 2;
	if (diff) memset(ss2, '0', diff);
	memcpy(ss2+diff, s, snumlen);
    }
  coda:
    newstr = rb_usascii_str_new(ss, sslen);
    xfree(ss);
    return newstr;
}

/*
 *  call-seq:
 *     dec.to_s   => string
 *
 *  *WARNING*: The behavior of this method may change.
 *
 *  Returns a string containing a simple representation of self.
 *
 *     Decimal(1).to_s             #=> "1"
 *     Decimal("1.1").to_s         #=> "1.1"
 *     Decimal::INFINITY.to_s   #=> "Infinity"
 */
static VALUE
dec_to_s(VALUE self)
{
    Decimal *d;

    CHECK_NAN_WITH_VAL(self, rb_usascii_str_new_cstr("NaN"));
    if (self == VALUE_PINF) return rb_usascii_str_new_cstr("Infinity");
    if (self == VALUE_NINF) return rb_usascii_str_new_cstr("-Infinity");
    GetDecimal(self, d);
    if (DEC_ZERO_P(d)) {
	const size_t HEAD_LEN = d->inum == DEC_PZERO ? 2U : 3U; /* "-0.".length */
	long len = HEAD_LEN + d->scale;
	char *buf;

	/* FIXME: use "0eN" style when the scale is negative? */
	if (d->scale <= 0) /* ignore the case of negative scale */
	    return d->inum == DEC_PZERO ?
	      rb_usascii_str_new_cstr("0") : rb_usascii_str_new_cstr("-0");
	buf = xmalloc(len);
	if (d->inum == DEC_PZERO)
	    memcpy(buf, "0.", HEAD_LEN);
	else
	    memcpy(buf, "-0.", HEAD_LEN);
	memset(buf + HEAD_LEN, '0', d->scale);
	return rb_usascii_str_new(buf, len);
    }
    return finite_to_s(d);
}

/*
 *  call-seq:
 *     dec.inspect   => string
 *
 *  Returns a easy-to-distinguish string: <code>"Decimal(#{dec})"</code>.
 *
 *     Decimal(1).inspect             #=> "Decimal(1)"
 *     Decimal("1.1").inspect         #=> "Decimal(1.1)"
 *     Decimal::INFINITY.inspect   #=> "Decimal(Infinity)"
 */
static VALUE
dec_inspect(VALUE self)
{
    char *s;
    VALUE str, newstr;
    long len;

    str = dec_to_s(self);
    len = 9 + RSTRING_LEN(str); /* 9 == strlen("Decimal()") */
    s = ALLOC_N(char, len + 1); /* +1 for NUL */
    sprintf(s, "Decimal(%s)", RSTRING_PTR(str));
    newstr = rb_usascii_str_new(s, len);
    xfree(s);
    return newstr;
}

/*
 *  call-seq:
 *     dec.coerce(other)   => array
 *
 *  Returns array <code>[Decimal(other), dec]</code> if _other_ has a
 *  compatible type, +Integer+ or +Decimal+.
 *  Otherwise raises a +TypeError+.
 *
 *     Decimal(1).coerce(2)            #=> [Decimal(2), Decimal(1)]
 *     Decimal(1).coerce(Decimal(2))   #=> [Decimal(2), Decimal(1)]
 *     Decimal(1).coerce(2.5)          #=> (TypeError)
 */
static VALUE
dec_coerce(VALUE x, VALUE y)
{
    VALUE yy;

    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	yy = WrapDecimal(inum_to_dec(y));
	return rb_assoc_new(yy, x);
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't coerce Float to Decimal; "
		 "use Decimal#to_f explicitly if needed");
	break;
      case T_DATA:
	if (DECIMAL_P(y)) return rb_assoc_new(y, x);
	/* fall through */
      default:
	rb_raise(rb_eTypeError, "can't coerce %s to Decimal",
		 rb_obj_classname(y));
	break;
    }
    return Qnil; /* not reached */
}

/*
 *  call-seq:
 *     -dec   => decimal
 *
 *  Returns a negated value of _dec_.
 */
static VALUE
dec_uminus(VALUE num)
{
    VALUE inum;
    Decimal *d;

    CHECK_NAN(num);
    if (num == VALUE_PINF) return VALUE_NINF;
    if (num == VALUE_NINF) return VALUE_PINF;

    GetDecimal(num, d);
    if (d->inum == DEC_PZERO)
	inum = DEC_NZERO;
    else if (d->inum == DEC_NZERO)
	inum = DEC_PZERO;
    else
	inum = INUM_UMINUS(d->inum);
    return WrapDecimal(dec_raw_new(inum, d->scale));
}

/* returns x * (10 ** n) */
static VALUE
inum_lshift(VALUE x, long n)
{
    VALUE y;

    if (n <= 0) rb_bug("inum_lshift(): not reached");
    y = fix_pow(INT2FIX(10), LONG2NUM(n));
    return INUM_MUL(x, y);
}

/* the "normal" number means "finite and non-zero" */
static Decimal *
normal_plus(Decimal *x, Decimal *y, const int add)
{
    VALUE inum;
    long scale;

    if (x->scale == y->scale) {
	inum = add ? INUM_PLUS(x->inum, y->inum)
                   : INUM_MINUS(x->inum, y->inum);
	scale = x->scale;
    }
    else {
	Decimal *max, *min;
	VALUE min_inum;

	if (x->scale > y->scale) max = x, min = y;
	else max = y, min = x;
	scale = max->scale;
	min_inum = inum_lshift(min->inum, max->scale - min->scale);
	if (add) inum = INUM_PLUS(min_inum, max->inum);
	else if (max == x) inum = INUM_MINUS(max->inum, min_inum);
	else inum = INUM_MINUS(min_inum, max->inum);
    }
    if (INUM_ZERO_P(inum)) inum = DEC_PZERO;
    return dec_raw_new(inum, scale);
}

#define MAX(x, y) ((x) > (y) ? (x) : (y))
/*
 *  call-seq:
 *     dec + other   => decimal
 *
 *  Returns a new decimal which is the sum of _dec_ and _other_.
 */
static VALUE
dec_plus(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2(x, y);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
	break;
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y, '+');
    }

    if (DEC_VALUE_ISINF(x)) {
	if (DEC_VALUE_ISINF(y) && x != y) return VALUE_NaN;
	return x;
    }
    if (DEC_VALUE_ISINF(y)) return y;
    /* now, x and y are not NaN nor +-INFINITY */
    GetDecimal(x, a);
    if (DEC_ZERO_P(a)) {
	VALUE inum;

        if (DEC_ZERO_P(b)) {
	    const long scale = MAX(a->scale, b->scale);

            if (a->inum == DEC_NZERO && b->inum == DEC_NZERO)
		return dec_nzero(scale);
            return dec_pzero(scale);
        }
	if (a->scale <= b->scale)
	    return y;
	inum = inum_lshift(b->inum, a->scale - b->scale);
	return WrapDecimal(dec_raw_new(inum, a->scale));
    }
    if (DEC_ZERO_P(b)) {
	VALUE inum;
	
	if (a->scale >= b->scale)
	    return x;
	inum = inum_lshift(a->inum, b->scale - a->scale);
	return WrapDecimal(dec_raw_new(inum, b->scale));
    }
    /* "true" means addition */
    return WrapDecimal(normal_plus(a, b, Qtrue));
}

#define NEGATE_INF(x) ((x) == VALUE_PINF ? VALUE_NINF : VALUE_PINF)
/*
 *  call-seq:
 *     dec - other   => decimal
 *
 *  Returns a new float which is the difference of _dec_ and _other_.
 */
static VALUE
dec_minus(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2(x, y);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
	break;
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y, '-');
    }
    if (DEC_VALUE_ISINF(x)) {
	if (x == y) return VALUE_NaN;
	return x;
    }
    if (DEC_VALUE_ISINF(y)) return NEGATE_INF(y);

    GetDecimal(x, a);
    if (DEC_ZERO_P(a)) { /* FIXME: needs refactoring */
	if (!DEC_ISINF(b) && DEC_ZERO_P(b) && a->inum == b->inum) {
	     /* FIXME: UNDER CONSTRUCTION for scaling */
	    return dec_pzero(MAX(a->scale, b->scale));
	}
	return dec_uminus(y);
    }
    if (DEC_ZERO_P(b)) return x;
    /* "false" means subtraction */
    return WrapDecimal(normal_plus(a, b, Qfalse));
}

static Decimal *
normal_mul(Decimal *x, Decimal *y)
{
    return dec_raw_new(INUM_MUL(x->inum, y->inum), x->scale + y->scale);
}

/*
 *  call-seq:
 *     dec * other   => decimal
 *
 *  Returns a new decimal which is the product of _dec_ and _other_.
 */
static VALUE
dec_mul(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2(x, y);
    switch (TYPE(y)) {
      case T_FIXNUM:
        /* TODO: can be optimized if y = 0, 1 or -1 */
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
        break;
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y, '*');
    }
    GetDecimal(x, a);

    if (DEC_ISINF(a)) {
	if (DEC_ISINF(b)) return x == y ? VALUE_PINF : VALUE_NINF;
	if (DEC_ZERO_P(b)) return VALUE_NaN;
	if (!INUM_NEGATIVE_P(b->inum)) return x;
	return dec_uminus(x);
    }
    if (DEC_ZERO_P(a)) {
	if (DEC_ISINF(b)) return VALUE_NaN;
	if (DEC_ZERO_P(b))  {
	    return a->inum == DEC_PZERO ? y : dec_uminus(y);
	}
	if (INUM_NEGATIVE_P(b->inum)) return dec_uminus(x);
	return x;
    }
    if (DEC_IMMEDIATE_P(b) || DEC_ZERO_P(b)) {
        if (INUM_NEGATIVE_P(a->inum)) return dec_uminus(y);
        return y;
    }
    return WrapDecimal(normal_mul(a, b));
}

static Decimal *
do_round(Decimal *d, long scale, VALUE mode, VALUE *inump)
{
    Decimal *d2;
    long diff;
    int lower;
    int trailing_nonzero, negative;
    VALUE inum, inumabs, shift, ary;

    if (d == DEC_PINF) rb_raise(eDomainError, "Infinity");
    if (d == DEC_NINF) rb_raise(eDomainError, "-Infinity");
    if (d == DEC_NaN) rb_raise(eDomainError, "NaN");
    if (INUM_SPZERO_P(d->inum)) {
	if (inump) {
	    if (scale > 0) rb_bug("do_round(): "
                                   "scale > 0 with Integer request");
	    *inump = INT2FIX(0);
	    return NULL;
	}
	d2 = finite_dup(d);
	if (d->scale > scale) d2->scale = scale;
	return d2;
    }
    if (d->scale <= scale) { /* no need to round */
	if (scale) return finite_dup(d); /* return Decimal */
	/* return Integer */
	if (!inump) /* XXX: may be reached when Decimal(1)/1  */
            rb_bug("do_round(): not reached[2]");
	/* FIXME: scaling policy, no need to grow scale? */
	if (d->scale == 0) *inump = d->inum;
	else *inump = inum_lshift(d->inum, -d->scale);
	return NULL;
    }
    negative = INUM_NEGATIVE_P(d->inum);
    diff = d->scale - scale;
    inumabs = negative ? INUM_UMINUS(d->inum) : d->inum;
    if (mode == ROUND_CEILING || /* don't need lower digit */
	mode == ROUND_DOWN ||
	mode == ROUND_FLOOR ||
	mode == ROUND_UP ||
	mode == ROUND_UNNECESSARY) {
	shift = inum_lshift(INT2FIX(1), diff);
	ary = INUM_DIVMOD(inumabs, shift);
	inum = RARRAY_PTR(ary)[0];
	if (mode == ROUND_DOWN) goto coda;
	trailing_nonzero = !INUM_ZERO_P(RARRAY_PTR(ary)[1]);
	if (mode == ROUND_CEILING) {
	    if (!negative && trailing_nonzero) INUM_INC(inum);
	}
	else if (mode == ROUND_FLOOR) {
	    if (negative && trailing_nonzero) INUM_INC(inum);
	}
	else if (mode == ROUND_UP) {
	    if (trailing_nonzero) INUM_INC(inum);
	}
	else { /* mode == ROUND_UNNECESSARY */
	    if (trailing_nonzero) {
		rb_raise(eArithmeticError, "rounding necessary");
	    }
	}
    }
    else if (mode == ROUND_HALF_DOWN || /* needs lower digit */
	     mode == ROUND_HALF_UP ||
	     mode == ROUND_HALF_EVEN) {
        if (diff > 1) { /* needs shift */
            shift = inum_lshift(INT2FIX(1), diff-1);
            inumabs = INUM_DIV(inumabs, shift);
        }
	ary = INUM_DIVMOD(inumabs, INT2FIX(10));
	inum = RARRAY_PTR(ary)[0];
	lower = FIX2INT(RARRAY_PTR(ary)[1]);
	if (mode == ROUND_HALF_DOWN) {
	    if (lower > 5) INUM_INC(inum);
	}
	else if (mode == ROUND_HALF_UP)	{
	    if (lower >= 5) INUM_INC(inum);
	}
	else { /* mode == ROUND_HALF_EVEN */
	    if (INUM_ODD_P(inum)) {
		if (lower >= 5) INUM_INC(inum);
	    }
	    else {
		if (lower > 5) INUM_INC(inum);
	    }
	}
    }
  coda:
    if (negative) inum = INUM_UMINUS(inum);
    if (scale <= 0 && inump != NULL) {
	/* return Integer */
	if (scale < 0) inum = inum_lshift(inum, -scale);
	*inump = inum;
	return NULL;
    }
    /* return Decimal */
    if (INUM_ZERO_P(inum)) {
	inum = negative ? DEC_NZERO : DEC_PZERO;
	scale = 0;
    }
    return dec_raw_new(inum, scale);
}

static Decimal *
normal_divide(Decimal *x, Decimal *y, long scale, VALUE mode)
{
    long diff, z_scale;
    VALUE xx;
    Decimal *z;

    diff = x->scale - y->scale;
    if (diff <= scale) {
	xx = inum_lshift(x->inum, scale-diff+1); /* +1 for rounding */
	z_scale = scale + 1;
    }
    else {
	/* FIXME: may be a bug...? */
	xx = x->inum;
	z_scale = diff;
    }
    z = dec_raw_new(INUM_DIV(xx, y->inum), z_scale);
    return do_round(z, scale, mode, NULL);
}

static int
valid_rounding_mode_p(VALUE sym)
{
    if (sym == ROUND_CEILING ||
        sym == ROUND_DOWN ||
        sym == ROUND_FLOOR ||
        sym == ROUND_HALF_DOWN ||
        sym == ROUND_HALF_EVEN ||
        sym == ROUND_HALF_UP ||
        sym == ROUND_UP ||
        sym == ROUND_UNNECESSARY) {
            return Qtrue;
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     dec.divide(other, scale=0, mode=Decimal::ROUND_UNNECESSARY)   #=> decimal or integer
 *
 *  *WARNING*: The behavior of this method may change.
 *
 *  Returns a new decimal which is the result of dividing _dec_ by _other_.
 *
 *  *FIXME*: write details
 */
static VALUE
dec_divide(int argc, VALUE *argv, VALUE x)
{
    VALUE y;
    Decimal *a, *b;
    VALUE mode = ROUND_UNNECESSARY;
    long l, scale = 0; /* FIXME: dummy 0 */
    VALUE vscale, vmode;

    CHECK_NAN(x);
    GetDecimal(x, a);

    rb_scan_args(argc, argv, "12", &y, &vscale, &vmode);
    switch (argc) {
      case 3:
	Check_Type(vmode, T_SYMBOL);
	if (!valid_rounding_mode_p(vmode)) {
	    rb_raise(rb_eArgError, "invalid rounding mode %s",
                     RSTRING_PTR(rb_inspect(vmode)));
	}
	mode = vmode;
	/* fall through */
      case 2:
	scale = NUM2LONG(vscale);
	break;
      case 1:
	if (mode != ROUND_UNNECESSARY) {
	    rb_raise(rb_eArgError, "scale number argument needed");
	}
    }
    CHECK_NAN(y);

    switch (TYPE(y)) {
      case T_FIXNUM:
        l = FIX2LONG(y);
        if (l == 0) {
            if (DEC_ISINF(a)) return x;
            if (DEC_ZERO_P(a)) return VALUE_NaN;
            return INUM_NEGATIVE_P(a->inum) ? VALUE_NINF : VALUE_PINF;
        }
        else if (l == 1) return x;
        else if (l == -1) return dec_uminus(x);
	/* fall through */
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
        return Qnil; /* not reached */
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
        return rb_num_coerce_bin(x, y, rb_intern("divide"));
    }

    if (DEC_ISINF(a)) {
	if (DEC_ISINF(b)) return VALUE_NaN;
	if (b->inum == DEC_PZERO) return x;
	if (b->inum == DEC_NZERO) return NEGATE_INF(x);
	return INUM_NEGATIVE_P(b->inum) ? NEGATE_INF(x) : x;
    }
    if (DEC_ZERO_P(a)) {
	if (b == DEC_PINF) return x;
	if (b == DEC_NINF) return dec_uminus(x);
	if (INUM_SPZERO_P(b->inum)) return VALUE_NaN;
	return INUM_NEGATIVE_P(b->inum) ? dec_uminus(x) : x;
    }
    if (DEC_ISINF(b)) {
	if (INUM_NEGATIVE_P(a->inum) == (b == DEC_NINF)) {
	    return dec_pzero(0); /* FIXME for scaling */
	}
	return dec_nzero(0); /* FIXME for scaling */
    }
    if (DEC_ZERO_P(b)) {
	if (INUM_NEGATIVE_P(a->inum) == (b->inum == DEC_NZERO)) {
	    return VALUE_PINF;
	}
	return VALUE_NINF;
    }
    return WrapDecimal(normal_divide(a, b, scale, mode));
}

#ifdef DEBUG
/* :nodoc: */
static VALUE
dec_div(VALUE x, VALUE y)
{
    return dec_divide(1, &y, x);
}
#endif

/* never accepts NaN for a and b */
static void
divmod(Decimal *a, Decimal *b, VALUE *divp, VALUE *modp)
{
    Decimal *div, *mod;
    Decimal *tmp;

    if (DEC_ISINF(a) || (!DEC_ISINF(b) && DEC_ZERO_P(b))) {
	if (divp) *divp = VALUE_NaN;
	if (modp) *modp = VALUE_NaN;
	return;
    }
    else if (INUM_SPZERO_P(a->inum)) {
	VALUE div_inum;

	if (b == DEC_NINF || (b != DEC_PINF && INUM_NEGATIVE_P(b->inum))) {
	    div_inum = DEC_NZERO;
	}
	else {
	    div_inum = DEC_PZERO;
	}
	div = dec_raw_new(div_inum, 0);
	mod = finite_dup(a);
    }
    else if (DEC_ISINF(b)) {
	const int a_negative = INUM_NEGATIVE_P(a->inum);
	VALUE div_inum;

	if (a_negative != (b == DEC_NINF)) { /* signs differ */
	    div_inum = INT2FIX(-1);
	    mod = b; /* FIXME: another infinity instance created! */
	}
	else {
	    div_inum = a_negative ? DEC_NZERO : DEC_PZERO;
 	    mod = finite_dup(a);
	}
	div = dec_raw_new(div_inum, 0);
    }
    else {
	/* both of a and b are finite and nonzero */
	div = normal_divide(a, b, 0, ROUND_DOWN); /* div = x / y */
	if (INUM_SPZERO_P(div->inum)) {
	    if (INUM_NEGATIVE_P(b->inum)) div->inum = DEC_NZERO;
	    mod = finite_dup(a);
	}
	else {
	    tmp = normal_mul(div, b); /* XXX */
	    mod = normal_plus(a, tmp, Qfalse); /* mod = x - div*y; */
	    xfree(tmp); /* XXX */
	}
	/* if ((mod < 0 && y > 0) || (mod > 0 && y < 0)) { */
	if (!INUM_SPZERO_P(mod->inum) && !INUM_SPZERO_P(b->inum) &&
            INUM_NEGATIVE_P(mod->inum) != INUM_NEGATIVE_P(b->inum)) {
	    mod = normal_plus(mod, b, Qtrue); /*  mod += y; */
	    INUM_DEC(div->inum); /* div -= 1; */
	}
    }
    if (divp) *divp = WrapDecimal(div);
    else if (!DEC_IMMEDIATE_P(div)) xfree(div);
    if (modp) *modp = WrapDecimal(mod);
    else if (!DEC_IMMEDIATE_P(mod)) xfree(mod);
}

/* :nodoc: */
static VALUE
dec_idiv(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE div;

    CHECK_NAN2(x, y);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y, rb_intern("div"));
    }
    GetDecimal(x, a);
    divmod(a, b, &div, NULL);
    return div;
}

/*
 *  call-seq:
 *     dec % other         => decimal
 *     dec.modulo(other)   => decimal
 *
 *  Return the modulo after division of _dec_ by _other_.
 *
 *     Decimal("6543.21").modulo(137)                 #=> Decimal(104.21)
 *     Decimal("6543.21").modulo(Decimal("137.24"))   #=> Decimal(92.9299999999996)
 */
static VALUE
dec_mod(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE mod;

    CHECK_NAN2(x, y);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y, '%');
    }
    GetDecimal(x, a);
    divmod(a, b, NULL, &mod);
    return mod;
}

/*
 *  call-seq:
 *     dec.divmod(other)   => array
 *
 *  Returns an array containing the quotient and modulus obtained by
 *  dividing _dec_ by _other_.
 *
 *     Decimal(11).divmod(3)                    #=> [Decimal(3), Decimal(2)]
 *     Decimal(11).divmod(-3)	                #=> [Decimal(-4), Decimal(-1)]
 *     Decimal(11).divmod(Decimal("3.5"))       #=> [Decimal(3), Decimal(0.5)]
 *     Decimal(-11).divmod(Decimal("3.5"))      #=> [Decimal(-4), Decimal(3.0)]
 *     Decimal("11.5").divmod(Decimal("3.5"))   #=> [Decimal(3), Decimal(1.0)]
 *
 *  See Numeric#divmod for more details.
 */
static VALUE
dec_divmod(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE div, mod;

    CHECK_NAN2_WITH_VAL(x, y, rb_assoc_new(VALUE_NaN, VALUE_NaN));
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y, rb_intern("divmod"));
    }
    GetDecimal(x, a);
    divmod(a, b, &div, &mod);
    return rb_assoc_new(div, mod);
}

static VALUE
power_with_fixnum(Decimal *x, VALUE y)
{
    VALUE inum;

    /* XXX: valid to rb_warn() out of here by rb_big_pow()? */
    inum = INUM_POW(x->inum, y);
    if (TYPE(inum) == T_FLOAT) /* got Infinity with warning, by too-big y */
        return VALUE_PINF;
    return WrapDecimal(dec_raw_new(inum, x->scale * FIX2LONG(y)));
}

/* TODO: implement dec ** otherdec */
/*
 *  call-seq:
 *     dec ** fix   => decimal
 *
 *  *WARNING*: The behavior of this method may change.
 *
 *  Raises _dec_ the _fix_ power.
 */
static VALUE
dec_pow(VALUE x, VALUE y)
{
    Decimal *a;
    long l;

    CHECK_NAN(x);
    Check_Type(y, T_FIXNUM);
    l = FIX2LONG(y);
    if (l < 0) rb_raise(rb_eArgError, "in a**b, b should be positive integer");
    if (l == 0) return WrapDecimal(dec_raw_new(INT2FIX(1), 0));
    if (l == 1) return x;

    if (x == VALUE_PINF) return x;
    if (x == VALUE_NINF) {
	return l % 2 == 0 ? VALUE_PINF : VALUE_NINF;
    }
    GetDecimal(x, a);
    if (a->inum == DEC_PZERO) return x;
    if (a->inum == DEC_NZERO) {
	return l % 2 == 0 ? dec_uminus(x) : x;
    }
    return power_with_fixnum(a, y);
}

static int
normal_cmp(Decimal *x, Decimal *y)
{
    if (INUM_NEGATIVE_P(x->inum) && !INUM_NEGATIVE_P(y->inum)) {
        return -1;
    }
    if (!INUM_NEGATIVE_P(x->inum) && INUM_NEGATIVE_P(y->inum)) {
        return 1;
    }
    if (x->scale == y->scale) return FIX2INT(INUM_CMP(x->inum, y->inum));
    /* XXX: can be optimized with INUM_EQ()? */
    if (x->scale < y->scale) {
        VALUE x_scaled = inum_lshift(x->inum, y->scale - x->scale);
        return FIX2INT(INUM_CMP(x_scaled, y->inum));
    }
    else {
        VALUE y_scaled = inum_lshift(y->inum, x->scale - y->scale);
        return FIX2INT(INUM_CMP(x->inum, y_scaled));
    }
}

/* never accepts NaN for x or y */
static int
cmp(Decimal *x, Decimal *y)
{
    if (x == y) return 0;
    if (x == DEC_PINF || y == DEC_NINF) return 1;
    if (x == DEC_NINF || y == DEC_PINF) return -1;
    if (INUM_SPZERO_P(x->inum)) {
	if (INUM_SPZERO_P(y->inum)) return 0;
	return INUM_NEGATIVE_P(y->inum) ? -1 : 1;
    }
    if (INUM_SPZERO_P(y->inum)) {
	return INUM_NEGATIVE_P(x->inum) ? -1 : 1;
    }
    return normal_cmp(x, y);
}

/*
 *  call-seq:
 *     dec == other   => true or false
 *
 *  Returns +true+ only if _other_ has the same value as _dec_.
 *  Contrast this with eql?, which requires _other_
 *  to be the same class, a +Decimal+.
 *
 *     Decimal(1) == 1                #=> true
 *     Decimal(1) == Decimal("1.0")   #=> true
 *     Decimal(1) == 1.0              #=> false
 */
static VALUE
dec_eq(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2_WITH_VAL(x, y, Qfalse);
    GetDecimal(x, a);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	return Qfalse;
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return RTEST(rb_num_coerce_cmp(x, y, rb_intern("==")));
    }
    return cmp(a, b) == 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec <=> other   => -1, 0, +1
 *
 *  Returns -1, 0, or +1 depending on whether _dec_ is less than,
 *  equal to, or greater than _other_. This is the basis for the
 *  tests in +Comparable+.
 */
static VALUE
dec_cmp(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2_WITH_VAL(x, y, Qnil);
    GetDecimal(x, a);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	return Qnil;
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_cmp(x, y, rb_intern("<=>"));
    }
    return INT2FIX(cmp(a, b));
}

/*
 *  call-seq:
 *     dec > other   => true or false
 *
 *  Returns +true+ if _dec_ is greater than _other_.
 */
static VALUE
dec_gt(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2_WITH_VAL(x, y, Qfalse);
    GetDecimal(x, a);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_cmperr(x, y);
        return Qnil; /* not reached */
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y, '>');
    }
    return cmp(a, b) > 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec >= other   => true or false
 *
 *  Returns +true+ if _dec_ is greater than or equal to _other_.
 */
static VALUE
dec_ge(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2_WITH_VAL(x, y, Qfalse);
    GetDecimal(x, a);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_cmperr(x, y);
        return Qnil; /* not reached */
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y, rb_intern(">="));
    }
    return cmp(a, b) >= 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec < other   => true or false
 *
 *  Returns +true+ if _dec_ is less than _other_.
 */
static VALUE
dec_lt(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2_WITH_VAL(x, y, Qfalse);
    GetDecimal(x, a);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_cmperr(x, y);
        return Qnil; /* not reached */
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y, '<');
    }
    return cmp(a, b) < 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec <= other   => true or false
 *
 *  Returns +true+ if _dec_ is less than or equal to _other_.
 */
static VALUE
dec_le(VALUE x, VALUE y)
{
    Decimal *a, *b;

    CHECK_NAN2_WITH_VAL(x, y, Qfalse);
    GetDecimal(x, a);
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_dec(y);
	break;
      case T_FLOAT:
	rb_cmperr(x, y);
        return Qnil; /* not reached */
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y, rb_intern("<="));
    }
    return cmp(a, b) <= 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec.eql?(other)   => true or false
 *
 *  Returns +true+ if _other_ is a +Decimal+ and is equal to _dec_
 *  including their values of scale.
 *
 *     Decimal(1) == 1                    #=> true
 *     Decimal(1).eql?(1)                 #=> false
 *     Decimal(1).eql?(Decimal(1))        #=> true
 *     Decimal(1).eql?(Decimal("1.0")))   #=> false
 */
static VALUE
dec_eql(VALUE x, VALUE y)
{
    Decimal *a, *b;

    if (TYPE(y) != T_DATA || !DECIMAL_P(y))
	return Qfalse;

    CHECK_NAN2_WITH_VAL(x, y, Qfalse);
    if (DEC_VALUE_ISINF(x) || DEC_VALUE_ISINF(y))
	return x == y ? Qtrue : Qfalse;

    GetDecimal(x, a);
    GetDecimal(y, b);
    if (a->scale != b->scale)
	return Qfalse;
    if (a->inum == b->inum)
	return Qtrue;
    if (INUM_SPZERO_P(a->inum) || INUM_SPZERO_P(b->inum))
	return Qfalse;
    if (INUM_EQ(a->inum, b->inum))
	return Qtrue;
    return Qfalse;
}

/*
 *  call-seq:
 *     dec.hash   => integer
 *
 *  Returns a hash code for _dec_.
 */
static VALUE
dec_hash(VALUE x)
{
    Decimal *d;
    long hash;

    GetDecimal(x, d);
    if (!DEC_IMMEDIATE_P(d)) {
        VALUE inum = d->inum; 

        if (INUM_SPZERO_P(inum)) inum = INT2FIX(0);
        hash = NUM2LONG(INUM_HASH(inum));
	hash ^= d->scale;
    }
    else hash = (long)d;
    return LONG2NUM(hash);
}

static Decimal *
dbl_threshold_to_dec(double threshold)
{
    VALUE v = flo_to_s(rb_float_new(threshold));
    Decimal *d = cstr_to_dec(StringValueCStr(v));

    if (!IMMEDIATE_P(d->inum)) {
	rb_global_variable(&d->inum);
    }
    return d;
}

static VALUE
dbl_threshold_to_inum(double threshold, VALUE *val)
{
    const double f = floor(threshold);

    if (FIXABLE(f)) {
	return *val = LONG2FIX((long)f);
    }
    rb_global_variable(val);
    return *val = rb_dbl2big(f);
}

static int
out_of_double_range_p(Decimal *d, double *f)
{
    Decimal *d_abs;
    int negative, out_of_range = Qfalse;

    if (!INUM_NEGATIVE_P(d->inum)) {
	negative = Qfalse;
	d_abs = d;
    }
    else {
	negative = Qtrue;
	d_abs = dec_raw_new(INUM_UMINUS(d->inum), d->scale);
    }

    if (normal_cmp(d_abs, GET_DEC_DBL_MIN()) < 0) { /* too small */
	*f = negative ? -0.0 : 0.0;
	out_of_range = Qtrue;
    }
    else if (normal_cmp(d_abs, GET_DEC_DBL_MAX()) > 0) { /* too big */
	*f = negative ? -INFINITY : INFINITY;
	out_of_range = Qtrue;
    }
    if (d_abs != d) xfree(d_abs);
    return out_of_range;
}

static double
normal_to_f(Decimal *d)
{
    double f;

    if (d->scale <= 0) {
	f = NUM2DBL(d->inum) * pow(10.0, -d->scale);
    }
    else { /* NUM2DBL() may warn */
	const int negative = INUM_NEGATIVE_P(d->inum);
	long scale = d->scale;
	VALUE inum_abs = negative ? INUM_UMINUS(d->inum) : d->inum;

	while (INUM_GT(inum_abs, GET_INUM_DBL_MAX())) {
	    inum_abs = INUM_DIV(inum_abs, INT2FIX(10));
	    scale--;
	}
	f = NUM2DBL(inum_abs) / pow(10.0, scale); /* scale may be negative */
	if (negative) f = -f;
    }
    return f;
}

/*
 *  call-seq:
 *     dec.to_f => float
 *
 *  Converts _dec_ to a +Float+.  Note that this may lose some precision
 *  and/or exactness.
 *  If you want to operate +Decimal+ with +Float+, use this method explicitly.
 */
static VALUE
dec_to_f(VALUE num)
{
    Decimal *d;
    double f;

    CHECK_NAN_WITH_VAL(num, rb_float_new(NAN));
    if (num == VALUE_PINF)
	return rb_float_new(INFINITY);
    if (num == VALUE_NINF)
	return rb_float_new(-INFINITY);

    GetDecimal(num, d);
    if (d->inum == DEC_PZERO)
	f = 0.0;
    else if (d->inum == DEC_NZERO)
	f = -0.0;
    else if (out_of_double_range_p(d, &f))
	rb_warning("Decimal out of Float range");
    else
	f = normal_to_f(d);

    return rb_float_new(f);
}

/*
 *  call-seq:
 *     dec.abs   => decimal
 *
 *  Returns the absolute value of _dec_.
 *
 *     Decimal("34.56").abs    #=> Decimal(34.56)
 *     Decimal("-34.56").abs   #=> Decimal(34.56)
 */
static VALUE
dec_abs(VALUE num)
{
    Decimal *d;
    VALUE inum;

    CHECK_NAN(num);
    if (DEC_VALUE_ISINF(num))
	return VALUE_PINF;
    GetDecimal(num, d);
    if (d->inum == DEC_PZERO ||
	(d->inum != DEC_NZERO && !INUM_NEGATIVE_P(d->inum))) {
	return num;
    }
    inum = (d->inum == DEC_NZERO) ? DEC_PZERO : INUM_UMINUS(d->inum);
    return WrapDecimal(dec_raw_new(inum, d->scale));
}

/*
 *  call-seq:
 *     dec.zero?   => true or false
 *
 *  Returns +true+ if _dec_ is zero.
 */
static VALUE
dec_zero_p(VALUE num)
{
    Decimal *d;

    GetDecimal(num, d);
    if (!DEC_IMMEDIATE_P(d) && DEC_ZERO_P(d)) {
        return Qtrue;
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     dec.to_i   => integer
 *
 *  Returns _dec_ truncated to an +Integer+.
 */
static VALUE
dec_to_i(VALUE num)
{
    Decimal *d;
    VALUE inum;

    GetDecimal(num, d);
    do_round(d, 0, ROUND_DOWN, &inum); /* equal to "d.round(0, :down)" */
    return inum;
 }

static VALUE
rounding_method(int argc, VALUE *argv, VALUE x, VALUE mode)
{
    Decimal *d;
    VALUE vscale, inum;
    long scale = 0;

    rb_scan_args(argc, argv, "01", &vscale);
    GetDecimal(x, d);
    if (argc == 1) scale = NUM2LONG(vscale);
    if (scale <= 0) {
	do_round(d, scale, mode, &inum);
	return inum;
    }
    return WrapDecimal(do_round(d, scale, mode, NULL));
}

/*
 *  call-seq:
 *     dec.truncate(n=0)   => integer or decimal
 *
 *  Returns _dec_ truncated to an +Integer+.
 *
 *  This is identical to <code>dec.round(n, Decimal::ROUND_DOWN)</code>.
 *  See <code>Decimal#round</code> for more details.
 */
static VALUE
dec_truncate(int argc, VALUE *argv, VALUE x)
{
    return rounding_method(argc, argv, x, ROUND_DOWN);
}

/*
 *  call-seq:
 *     dec.floor(n=0)   => integer or decimal
 *
 *  Returns the largest integer less than or equal to _dec_.
 *
 *     Decimal("1.2").floor    #=> 1
 *     Decimal("2.0").floor    #=> 2
 *     Decimal("-1.2").floor   #=> -2
 *     Decimal("-2.0").floor   #=> -2
 *
 *  This is identical to <code>dec.round(n, Decimal::ROUND_FLOOR)</code>.
 *  See <code>Decimal#round</code> for more details.
 */
static VALUE
dec_floor(int argc, VALUE *argv, VALUE x)
{
    return rounding_method(argc, argv, x, ROUND_FLOOR);
}

/*
 *  call-seq:
 *     dec.ceil(n=0)   => integer or decimal
 *
 *  Returns the smallest +Integer+ greater than or equal to _dec_.
 *
 *     Decimal("1.2").ceil    #=> 2
 *     Decimal("2.0").ceil    #=> 2
 *     Decimal("-1.2").ceil   #=> -1
 *     Decimal("-2.0").ceil   #=> -2
 *
 *  This is identical to <code>dec.round(n, Decimal::ROUND_CEILING)</code>.
 *  See <code>Decimal#round</code> for more details.
 */
static VALUE
dec_ceil(int argc, VALUE *argv, VALUE x)
{
    return rounding_method(argc, argv, x, ROUND_CEILING);
}

/*
 *  call-seq:
 *     dec.round(n=0, mode=Decimal::ROUND_HALF_UP)   => integer or decimal
 *
 *  *FIXME*: more examples 
 *
 *  Rounds _dec_ to a given precision _n_ in decimal digits (default 0 digits)
 *  with rounding mode _mode_.  Precision may be negative.  Returns a
 *  +Decimal+ when _n_ is greater than 0, +Integer+ otherwise.
 *
 *     Decimal("1.5").round    #=> 2
 *     Decimal("-1.5").round   #=> -2
 */
static VALUE
dec_round(int argc, VALUE *argv, VALUE x)
{
    Decimal *d;
    VALUE vscale, mode;
    long scale = 0;

    rb_scan_args(argc, argv, "02", &vscale, &mode);
    switch (argc) {
      case 2:
	Check_Type(mode, T_SYMBOL);
	if (!valid_rounding_mode_p(mode)) {
	    rb_raise(rb_eArgError, "invalid rounding mode %s",
                     RSTRING_PTR(rb_inspect(mode)));
	}
	/* fall through */
      case 1:
	scale = NUM2LONG(vscale);
	/* fall through */
      default:
        if (NIL_P(mode)) mode = ROUND_HALF_UP;
	break;
    }
    GetDecimal(x, d);
    if (scale <= 0) {
	VALUE inum;

	do_round(d, scale, mode, &inum);
	return inum;
    }
    return WrapDecimal(do_round(d, scale, mode, NULL));
}


/*
 *  call-seq:
 *     dec.nan?   => true or false
 *
 *  Returns +true+ if _dec_ is an invalid point number, NaN.
 *
 *     Decimal(-1).nan?            #=> false
 *     Decimal(1).divide(0).nan?   #=> false
 *     Decimal(0).divide(0).nan?   #=> true
 */
static VALUE
dec_nan_p(VALUE num)
{
    return num == VALUE_NaN;
}

/*
 *  call-seq:
 *     dec.finite?   => true or false
 *
 *  Returns +true+ if _dec_ is a finite number (it is not infinite
 *  nor NaN).
 *
 *     Decimal(0).finite?             #=> true
 *     Decimal(1).divide(0).finite?   #=> false
 *     Decimal(0).divide(0).finite?   #=> false
 */
static VALUE
dec_finite_p(VALUE num)
{
    if (!DEC_VALUE_ISINF(num) && num != VALUE_NaN) {
	return Qtrue;
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     dec.infinite?   => nil, -1, +1
 *
 *  Returns +nil+, -1, or +1 depending on whether _dec_ is finite,
 *  -infinity, or +infinity.
 *
 *     Decimal(0).infinite?              #=> nil
 *     Decimal(-1).divide(0).infinite?   #=> -1
 *     Decimal(+1).divide(0).infinite?   #=> 1
 */
static VALUE
dec_infinite_p(VALUE num)
{
    if (num == VALUE_PINF) return INT2FIX(1);
    if (num == VALUE_NINF) return INT2FIX(-1);
    return Qnil;
}

/*
 *  +Decimal+ is a decimal fraction that holds exact number in the decimal
 *  system unlike +Float+.  It can hold multi-precision digits, so you can
 *  calculate any detailed number as you likes.
 *
 *  *FIXME*: write exceptions raised by Float and details about scales
 */
void
Init_decimal(void)
{
    cDecimal = rb_define_class("Decimal", rb_cNumeric);
    /*  Raised when infinite or NaN was rounded. */
    eDomainError = rb_define_class_under(cDecimal, "DomainError",
					 rb_eRangeError);
    /*
     *  Raised when rounding is necessary in spite of a constant
     *  <code>Decimal::ROUND_UNNECESSARY</code> was passed to
     *  <code>Decimal#round</code>.
     */
    eArithmeticError = rb_define_class_under(cDecimal, "ArithmeticError",
					     rb_eStandardError);

    rb_define_global_function("Decimal", f_decimal, 1);

    rb_define_alloc_func(cDecimal, dec_s_allocate);
    rb_define_method(cDecimal, "initialize", dec_initialize, 1);

    /* static objects, should not be freed */
    VALUE_PINF = WrapStatic(DEC_PINF);
    VALUE_NINF = WrapStatic(DEC_NINF);
    VALUE_NaN = WrapStatic(DEC_NaN);
    /* and register them with GC */
    rb_global_variable(&VALUE_PINF);
    rb_global_variable(&VALUE_NINF);
    rb_global_variable(&VALUE_NaN);
    /* then define as constants */
    rb_define_const(cDecimal, "INFINITY", VALUE_PINF);
    rb_define_const(cDecimal, "NAN", VALUE_NaN);

    /* generated by:
     * %w(ceiling down floor half_down half_even half_up up
     *    unnecessary).each do |s|
     *   r = "ROUND_#{s.upcase}"
     *   puts %(#{r} = ID2SYM(rb_intern("#{s}"));)
     *   puts %(rb_define_const(cDecimal, "#{r}", #{r});)
     * end
     */
    ROUND_CEILING = ID2SYM(rb_intern("ceiling"));
    rb_define_const(cDecimal, "ROUND_CEILING", ROUND_CEILING);
    ROUND_DOWN = ID2SYM(rb_intern("down"));
    rb_define_const(cDecimal, "ROUND_DOWN", ROUND_DOWN);
    ROUND_FLOOR = ID2SYM(rb_intern("floor"));
    rb_define_const(cDecimal, "ROUND_FLOOR", ROUND_FLOOR);
    ROUND_HALF_DOWN = ID2SYM(rb_intern("half_down"));
    rb_define_const(cDecimal, "ROUND_HALF_DOWN", ROUND_HALF_DOWN);
    ROUND_HALF_EVEN = ID2SYM(rb_intern("half_even"));
    rb_define_const(cDecimal, "ROUND_HALF_EVEN", ROUND_HALF_EVEN);
    ROUND_HALF_UP = ID2SYM(rb_intern("half_up"));
    rb_define_const(cDecimal, "ROUND_HALF_UP", ROUND_HALF_UP);
    ROUND_UP = ID2SYM(rb_intern("up"));
    rb_define_const(cDecimal, "ROUND_UP", ROUND_UP);
    ROUND_UNNECESSARY = ID2SYM(rb_intern("unnecessary"));
    rb_define_const(cDecimal, "ROUND_UNNECESSARY", ROUND_UNNECESSARY);

#ifdef DEBUG
    rb_define_method(cDecimal, "scale", dec_scale, 0);
    rb_define_method(cDecimal, "unscaled_value", dec_unscaled_value, 0);
    rb_define_method(cDecimal, "strip_trailing_zeros",
		     dec_strip_trailing_zeros, 0);
    rb_define_method(cDecimal, "strip", dec_strip_trailing_zeros, 0);
#endif

    rb_define_method(cDecimal, "to_s", dec_to_s, 0);
    rb_define_method(cDecimal, "inspect", dec_inspect, 0);
    rb_define_method(cDecimal, "coerce", dec_coerce, 1);
    rb_define_method(cDecimal, "-@", dec_uminus, 0);
    rb_define_method(cDecimal, "+", dec_plus, 1);
    rb_define_method(cDecimal, "-", dec_minus, 1);
    rb_define_method(cDecimal, "*", dec_mul, 1);
    rb_define_method(cDecimal, "divide", dec_divide, -1);
#ifdef DEBUG
    rb_define_method(cDecimal, "/", dec_div, 1);
#endif
    rb_define_method(cDecimal, "div", dec_idiv, 1);
    rb_define_method(cDecimal, "%", dec_mod, 1);
    rb_define_method(cDecimal, "modulo", dec_mod, 1);
    rb_define_method(cDecimal, "divmod", dec_divmod, 1);
    rb_define_method(cDecimal, "**", dec_pow, 1);
    rb_define_method(cDecimal, "==", dec_eq, 1);
    rb_define_method(cDecimal, "<=>", dec_cmp, 1);
    rb_define_method(cDecimal, ">", dec_gt, 1);
    rb_define_method(cDecimal, ">=", dec_ge, 1);
    rb_define_method(cDecimal, "<", dec_lt, 1);
    rb_define_method(cDecimal, "<=", dec_le, 1);
    rb_define_method(cDecimal, "eql?", dec_eql, 1);
    rb_define_method(cDecimal, "hash", dec_hash, 0);
    rb_define_method(cDecimal, "to_f", dec_to_f, 0);
    rb_define_method(cDecimal, "abs", dec_abs, 0);
    rb_define_method(cDecimal, "zero?", dec_zero_p, 0);

    rb_define_method(cDecimal, "to_i", dec_to_i, 0);
    rb_define_method(cDecimal, "truncate",  dec_truncate, -1);
    rb_define_method(cDecimal, "floor", dec_floor, -1);
    rb_define_method(cDecimal, "ceil", dec_ceil, -1);
    rb_define_method(cDecimal, "round", dec_round, -1);

    rb_define_method(cDecimal, "nan?", dec_nan_p, 0);
    rb_define_method(cDecimal, "finite?", dec_finite_p, 0);
    rb_define_method(cDecimal, "infinite?", dec_infinite_p, 0);
}
