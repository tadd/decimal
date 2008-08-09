/*
 *  decimal.c - implementation of Decimal,
 *              a multi-precision decimal arithmetic library
 *
 *  Copyright (C) 2003-2008 Tadashi Saito
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the Ruby License. See the file "COPYING" for
 *  more details.
 */

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <ruby.h>
#include <rubysig.h>
#include <version.h>
#include <util.h>

/*
 * unfortunately, few copies of Integer functions
 * are needed from original Ruby
 */
#include "inum18.h"

/*
 * INUM_* macros: receive both Fixnum and Bignum,
 *		  to operate Integers transparently
 */
#define INUM_PLUS(a, b) \
    (FIXNUM_P(a) ? fix_plus(a, b) : rb_big_plus(a, b))
#define INUM_MINUS(a, b) \
    (FIXNUM_P(a) ? fix_minus(a, b) : rb_big_minus(a, b))
#define INUM_MUL(a, b) \
    (FIXNUM_P(a) ? fix_mul(a, b) : rb_big_mul(a, b))
#define INUM_DIV(a, b) \
    (FIXNUM_P(a) ? fix_div(a, b) : RARRAY(rb_big_divmod(a, b))->ptr[0])
#define INUM_DIVMOD(a, b) \
    (FIXNUM_P(a) ? fix_divmod(a, b) : rb_big_divmod(a, b))
#define INUM_POW(a, b) \
    (FIXNUM_P(a) ? fix_pow(a, b) : rb_big_pow(a, b))
#define INUM_EQ(a, b) \
    (FIXNUM_P(a) ? fix_equal(a, b) : rb_big_eq(a, b))
#define INUM_CMP(a, b) \
    (FIXNUM_P(a) ? fix_cmp(a, b) : rb_big_cmp(a, b))
#define INUM_UMINUS(n) \
    (FIXNUM_P(n) ? LONG2NUM(-FIX2LONG(n)) : big_uminus(n))
#define INUM_HASH(n) \
    (FIXNUM_P(n) ? LONG2NUM((long)n) : rb_big_hash(n))
#define INUM2STR(n) \
    (FIXNUM_P(n) ? rb_fix2str(n, 10) : rb_big2str(n, 10))

#define INUM_INC(n) do { n = INUM_PLUS(n, INT2FIX(1)); } while (0)
#define INUM_DEC(n) do { n = INUM_MINUS(n, INT2FIX(1)); } while (0)
#define INUM_ZERO_P(n) (FIXNUM_P(n) && FIX2LONG(n) == 0)
#define INUM_NEGATIVE_P(n) (FIXNUM_P(n) ? FIX2LONG(n) < 0 : !RBIGNUM(n)->sign)
#define INUM_BOTTOMDIG(n) (FIXNUM_P(n) ? FIX2LONG(n) % 10 : \
  !BIGZEROP(n) ? FIX2INT(RARRAY(rb_big_divmod(n, INT2FIX(10)))->ptr[1]) : 0)
#define INUM_ODD_P(n) (FIXNUM_P(n) ? n & 2 : BDIGITS(n)[0] & 1)

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

/* special constants - i.e. non-zero and non-fixnum */
/* used for signed zeros that never meet any fixnums nor normal VALUEs */
static const VALUE PZERO = 2, NZERO = 6;
#define dec_pzero() WrapDecimal(inum_to_dec(PZERO))
#define dec_nzero() WrapDecimal(inum_to_dec(NZERO))

#define DEC_ISINF(d) ((d) == DEC_PINF || (d) == DEC_NINF)
/* immediate means non-finite */
#define DEC_IMMEDIATE_P(d) (DEC_ISINF(d) || (d) == DEC_NaN)
/* special signed zeros */
#define DEC_ZERO_P(d) ((d)->inum == PZERO || (d)->inum == NZERO)
#define INUM_SPZERO_P(n) ((n) == PZERO || (n) == NZERO)

/* all rounding modes, corresponding to DEF_ROUNDING_MODE() */
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

static Decimal *
inum_to_dec(VALUE x)
{
    Decimal *d = ALLOC(Decimal);

    d = ALLOC(Decimal);
    if (INUM_ZERO_P(x)) d->inum = PZERO;
    else d->inum = x;
    d->scale = 0;
    return d;
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
    Decimal *d;
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

#if RUBY_RELEASE_CODE == 20070924 /* bug workaround for 1.8.6-p111 */
        if (ss == s || ss[-1] != '0') goto out;
        p = ss;
        do {
            p--;  
        } while (*p == '0' && p > s);
        if (*p == '0' || *p == '-' || *p == '+' || ISSPACE(*p))
            *ss = '0';
        else
  out:
#endif
	*ss = '_'; /* so that rb_cstr_to_inum() can ignore '.' */
	for (p = ss + 1; ISDIGIT(*p) || *p == '_'; p++) {
	    if (ISDIGIT(*p)) scale++;
	}
    }
    inum = rb_rescue(cstr_to_inum, (VALUE)s, invalid_str, (VALUE)assoc);
    d = ALLOC(Decimal);
    if (INUM_ZERO_P(inum)) {
	d->inum = strchr(s, '-') ? NZERO : PZERO;
    }
    else d->inum = inum;
    d->scale = scale;
    xfree(s);
    return d;
}

static Decimal *
finite_dup(Decimal *d)
{
    Decimal *d2 = ALLOC(Decimal);

    *d2 = *d;
    if (!FIXNUM_P(d->inum) && !INUM_SPZERO_P(d->inum)) {
	d2->inum = rb_big_clone(d->inum); /* inum is a Bignum */
    }
    return d2;
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
      case T_DATA:
	if (DECIMAL_P(arg)) {
	    Decimal *d;

	    GetDecimal(arg, d);
	    if (DEC_IMMEDIATE_P(d))
		return d;
	    return finite_dup(d);
	}
	/* fall through */
      case T_FLOAT:
	rb_raise(rb_eArgError, "invalid type Float: %s",
                 RSTRING(rb_inspect(arg))->ptr);
      default:
	rb_raise(rb_eArgError, "invalid value for Decimal: %s",
                 RSTRING(rb_inspect(arg))->ptr);
    }
    return NULL; /* not reached */
}

static VALUE
f_decimal(VALUE klass_unused, VALUE arg)
{
    if (DECIMAL_P(arg)) return arg;
    return WrapDecimal(create_dec(arg));
}

/* TODO: should know about allocation framework for dumping/loading */
static VALUE
dec_s_allocate(VALUE klass)
{
    return Data_Wrap_Struct(klass, dec_mark, dec_free, NULL);
}

/* TODO: check whether arg is a Decimal or not */
static VALUE
dec_initialize(VALUE self, VALUE arg)
{
    Decimal *d = create_dec(arg);

    if (DEC_IMMEDIATE_P(d)) { /* no need to manage about memory */
        RDATA(self)->dmark = RDATA(self)->dfree = NULL;
    }
    DATA_PTR(self) = d;
    return self;
}

#ifdef DEBUG
static VALUE
dec_scale(VALUE self)
{
    Decimal *d;

    GetDecimal(self, d); 
    if (DEC_IMMEDIATE_P(d)) return Qnil;
    return LONG2NUM(d->scale);
}

static VALUE
dec_unscaled_value(VALUE self)
{
    Decimal *d;

    GetDecimal(self, d);
    if (DEC_IMMEDIATE_P(d)) return Qnil;
    return DEC_ZERO_P(d) ? INT2FIX(0) : d->inum;
}

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
    d2 = ALLOC(Decimal);
    *d2 = *d;
    /* TODO: can be optimized with dividing each part
     * for Bignums and Fixnums */
    while (INUM_BOTTOMDIG(d2->inum) == 0) {
	d2->inum = INUM_DIV(d2->inum, INT2FIX(10));
	d2->scale--;
    }
    return WrapDecimal(d2);
}
#endif

/* FIXME: should return "%g" format string */
static VALUE
finite_to_s(Decimal *d)
{
    const VALUE str = INUM2STR(d->inum);
    const char *s = RSTRING(str)->ptr;
    const long slen = RSTRING(str)->len;
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
    newstr = rb_str_new(ss, sslen);
    xfree(ss);
    return newstr;
}

static VALUE
dec_to_s(VALUE self)
{
    Decimal *d;

    GetDecimal(self, d);
    if (d == DEC_NaN) return rb_str_new2("NaN");
    if (d == DEC_PINF) return rb_str_new2("Infinity");
    if (d == DEC_NINF) return rb_str_new2("-Infinity");
    if (d->inum == PZERO) return rb_str_new2("0");
    if (d->inum == NZERO) return rb_str_new2("-0");
    return finite_to_s(d);
}

static VALUE
dec_inspect(VALUE self)
{
    char *s;
    VALUE str, newstr;
    long len;

    str = dec_to_s(self);
    len = 9 + RSTRING(str)->len; /* 9 == strlen("Decimal()") */
    s = ALLOC_N(char, len + 1); /* +1 for NUL */
    sprintf(s, "Decimal(%s)", RSTRING(str)->ptr);
    newstr = rb_str_new(s, len);
    xfree(s);
    return newstr;
}

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

static VALUE
dec_uminus(VALUE num)
{
    Decimal *d, *d2;

    GetDecimal(num, d);
    if (d == DEC_NaN) return num;
    if (d == DEC_PINF) return VALUE_NINF;
    if (d == DEC_NINF) return VALUE_PINF;

    d2 = ALLOC(Decimal);
    d2->scale = d->scale;
    if (d->inum == PZERO)
	d2->inum = NZERO;
    else if (d->inum == NZERO)
	d2->inum = PZERO;
    else
	d2->inum = INUM_UMINUS(d->inum);
    return WrapDecimal(d2);
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

/* the "normal" number means "finite and nonzero" */
static Decimal *
normal_plus(Decimal *x, Decimal *y, const int add)
{
    Decimal *z;
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
    if (INUM_ZERO_P(inum)) inum = PZERO;
    z = ALLOC(Decimal);
    z->inum = inum;
    z->scale = scale;
    return z;
}

static VALUE
dec_plus(VALUE x, VALUE y)
{
    Decimal *a, *b;

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
	    if (b == DEC_NaN) return VALUE_NaN;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y);
    }
    GetDecimal(x, a);
    if (a == DEC_NaN) return VALUE_NaN;
    
    /* now, x and y are not NaNs */
    if (DEC_ISINF(a)) {
	if (DEC_ISINF(b) && a != b) return VALUE_NaN;
	return x;
    }
    if (DEC_ZERO_P(a)) {
        if (!DEC_IMMEDIATE_P(b) && DEC_ZERO_P(b)) { /* XXX */
            if (a->inum == NZERO && b->inum == NZERO)
                return dec_nzero(); /* FIXME: scale policy for 0? */
            return dec_pzero(); /* FIXME: ditto */
        }
        return y;
    }
    if (DEC_ISINF(b)) return y;
    if (DEC_ZERO_P(b)) return x; /* FIXME: ditto */
    /* "true" means addition */
    return WrapDecimal(normal_plus(a, b, Qtrue));
}

static VALUE
dec_minus(VALUE x, VALUE y)
{
    Decimal *a, *b;

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
	    if (b == DEC_NaN) return VALUE_NaN;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y);
    }
    GetDecimal(x, a);
    if (a == DEC_NaN) return VALUE_NaN;

    if (DEC_ISINF(a)) {
	if (a == b) return VALUE_NaN;
	return x;
    }
    if (DEC_ZERO_P(a)) { /* FIXME: need to refactor */
	if (!DEC_ISINF(b) && DEC_ZERO_P(b) && a->inum == b->inum) {
	    return dec_pzero(); /* FIXME: for scaling */
	}
	return dec_uminus(y);
    }
    if (DEC_ISINF(b)) return dec_uminus(y);
    if (DEC_ZERO_P(b)) return x;
    /* "false" means subtraction */
    return WrapDecimal(normal_plus(a, b, Qfalse));
}

static Decimal *
normal_mul(Decimal *x, Decimal *y)
{
    Decimal *z = ALLOC(Decimal);

    z->inum = INUM_MUL(x->inum, y->inum);
    z->scale = x->scale + y->scale;
    return z;
}

static VALUE
dec_mul(VALUE x, VALUE y)
{
    Decimal *a, *b;

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
	    if (b == DEC_NaN) return VALUE_NaN;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y);
    }
    GetDecimal(x, a);
    if (a == DEC_NaN) return VALUE_NaN;

    if (DEC_ISINF(a)) {
	if (DEC_ISINF(b)) return a == DEC_PINF ? y : dec_uminus(y);
	if (DEC_ZERO_P(b)) return VALUE_NaN;
	if (!INUM_NEGATIVE_P(b->inum)) return x;
	return dec_uminus(x);
    }
    if (DEC_ZERO_P(a)) {
	if (DEC_ISINF(b)) return VALUE_NaN;
	if (DEC_ZERO_P(b))  {
	    return a->inum == PZERO ? y : dec_uminus(y);
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
	inum = RARRAY(ary)->ptr[0];
	if (mode == ROUND_DOWN) goto coda;
	trailing_nonzero = !INUM_ZERO_P(RARRAY(ary)->ptr[1]);
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
	inum = RARRAY(ary)->ptr[0];
	lower = FIX2INT(RARRAY(ary)->ptr[1]);
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
    if (scale <= 0 && inump) { /* return Integer */
	if (scale < 0) inum = inum_lshift(inum, -scale);
	*inump = inum;
	return NULL;
    }
    /* return Decimal */
    d2 = ALLOC(Decimal);
    if (INUM_ZERO_P(inum)) {
	d2->inum = negative ? NZERO : PZERO;
	d2->scale = 0;
    }
    else {
	d2->inum = inum;
	d2->scale = scale;
    }
    return d2;
}

static Decimal *
normal_divide(Decimal *x, Decimal *y, long scale, VALUE mode)
{
    long diff;
    VALUE xx;
    Decimal *z = ALLOC(Decimal);

    diff = x->scale - y->scale;
    if (diff <= scale) {
	xx = inum_lshift(x->inum, scale-diff+1); /* +1 for rounding */
	z->scale = scale + 1;
    }
    else {
	/* FIXME: may be a bug...? */
	xx = x->inum;
	z->scale = diff;
    }
    z->inum = INUM_DIV(xx, y->inum);
    return do_round(z, scale, mode, NULL);
}

static int
valid_rounding_mode(VALUE sym)
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

static VALUE
dec_divide(int argc, VALUE *argv, VALUE x)
{
    VALUE y;
    Decimal *a, *b;
    VALUE mode = ROUND_UNNECESSARY;
    long l, scale = 0; /* FIXME: dummy 0 */
    VALUE vscale, vmode;

    GetDecimal(x, a);
    if (a == DEC_NaN) return VALUE_NaN; /* no need to check b */

    rb_scan_args(argc, argv, "12", &y, &vscale, &vmode);
    switch (argc) {
      case 3:
	Check_Type(vmode, T_SYMBOL);
	if (!valid_rounding_mode(vmode)) {
	    rb_raise(rb_eArgError, "invalid rounding mode %s",
                     RSTRING(rb_inspect(mode))->ptr);
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

    switch (TYPE(y)) {
      case T_FIXNUM:
        l = FIX2LONG(y);
        if (l == 0) {
            if (DEC_ZERO_P(a)) return VALUE_NaN;
            if (DEC_ISINF(a)) return x;
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
	    if (b == DEC_NaN) return VALUE_NaN;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y);
    }
    /* TODO: can be optimized if b == 0, 1 or -1 */
    if (DEC_ISINF(a)) {
	if (DEC_ISINF(b)) return VALUE_NaN;
	if (b->inum == PZERO) return x;
	if (b->inum == NZERO) return dec_uminus(x);
	return INUM_NEGATIVE_P(b->inum) ? dec_uminus(x) : x;
    }
    if (DEC_ZERO_P(a)) {
	if (b == DEC_PINF) return x;
	if (b == DEC_NINF) return dec_uminus(x);
	if (INUM_SPZERO_P(b->inum)) return VALUE_NaN;
	return INUM_NEGATIVE_P(b->inum) ? dec_uminus(x) : x;
    }
    if (DEC_ISINF(b)) {
	if (INUM_NEGATIVE_P(a->inum) == (b == DEC_NINF)) {
	    return dec_pzero();
	}
	return dec_nzero();
    }
    if (DEC_ZERO_P(b)) {
	if (INUM_NEGATIVE_P(a->inum) == (b->inum == NZERO)) {
	    return VALUE_PINF;
	}
	return VALUE_NINF;
    }
    return WrapDecimal(normal_divide(a, b, scale, mode));
}

#ifdef DEBUG
static VALUE
dec_div(VALUE x, VALUE y)
{
    return dec_divide(1, &y, x);
}
#endif

/*
 * FIXME: test needed!
 */
static void
divmod(Decimal *a, Decimal *b, VALUE *divp, VALUE *modp)
{
    Decimal *div, *mod;
    Decimal *tmp;

    if (a == DEC_NaN || DEC_ISINF(a) || b == DEC_NaN ||
	(!DEC_ISINF(b) && DEC_ZERO_P(b))) {
	div = mod = DEC_NaN;
    }
    else if (INUM_SPZERO_P(a->inum)) {
	div = ALLOC(Decimal);
	div->scale = 0;
	if (b == DEC_NINF || (b != DEC_PINF && INUM_NEGATIVE_P(b->inum))) {
	    div->inum = NZERO;
	}
	else {
	    div->inum = PZERO;
	}
	mod = finite_dup(a);
    }
    else if (DEC_ISINF(b)) {
	const int a_negative = INUM_NEGATIVE_P(a->inum);

	div = ALLOC(Decimal);
	div->scale = 0;
	if (a_negative != (b == DEC_NINF)) { /* signs differ */
	    div->inum = INT2FIX(-1);
	    mod = b;
	}
	else {
	    div->inum = a_negative ? NZERO : PZERO;
 	    mod = finite_dup(a);
	}
    }
    else {
	/* both of a and b are finite and nonzero */
	div = normal_divide(a, b, 0, ROUND_DOWN); /* div = x / y */
	if (INUM_SPZERO_P(div->inum)) {
	    if (INUM_NEGATIVE_P(b->inum)) div->inum = NZERO;
	    mod = finite_dup(a);
	}
	else {
	    tmp = normal_mul(div, b); /* XXX */
	    mod = normal_plus(a, tmp, Qfalse); /* mod = x - div*y; */
	    xfree(tmp); /* XXX */
	}
	/* if ((mod < 0 && y > 0) || (mod > 0 && y < 0)) { */
	if (INUM_NEGATIVE_P(mod->inum) != INUM_NEGATIVE_P(b->inum) &&
            !INUM_SPZERO_P(mod->inum) && !INUM_SPZERO_P(b->inum)) {
	    mod = normal_plus(mod, b, Qtrue); /*  mod += y; */
	    INUM_DEC(div->inum); /* div -= 1; */
	}
    }
    if (divp) *divp = WrapDecimal(div);
    else if (!DEC_IMMEDIATE_P(div)) xfree(div);
    if (modp) *modp = WrapDecimal(mod);
    else if (!DEC_IMMEDIATE_P(mod)) xfree(mod);
}

static VALUE
dec_idiv(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE div;

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
	return rb_num_coerce_bin(x, y);
    }
    GetDecimal(x, a);
    divmod(a, b, &div, NULL);
    return div;
}

static VALUE
dec_mod(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE mod;

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
	return rb_num_coerce_bin(x, y);
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
 *  See +Numeric#divmod+ for more details.
 */
static VALUE
dec_divmod(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE div, mod;

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
	return rb_num_coerce_bin(x, y);
    }
    GetDecimal(x, a);
    divmod(a, b, &div, &mod);
    return rb_assoc_new(div, mod);
}

static VALUE
power_with_fixnum(Decimal *x, VALUE y)
{
    Decimal *d;
    VALUE inum;

    /* XXX: it's valid to rb_warn() out of here, rb_big_pow()? */ 
    inum = INUM_POW(x->inum, y);
    if (TYPE(inum) == T_FLOAT) /* got Infinity with warning, by too-big y */
        return VALUE_PINF;
    d = ALLOC(Decimal);
    d->inum = inum;
    d->scale = x->scale * FIX2LONG(y);
    return WrapDecimal(d);
}

/*
 *  call-seq:
 *     dec ** fix   => decimal
 *
 *  Raises _dec_ the _fix_ power.
 */
static VALUE
dec_pow(VALUE x, VALUE y)
{
    Decimal *a;
    long l;

    GetDecimal(x, a);
    Check_Type(y, T_FIXNUM);
    l = FIX2LONG(y);
    if (l < 0) rb_raise(rb_eArgError, "in a**b, b should be positive integer");
    if (l == 0) {
	Decimal *d = ALLOC(Decimal);

	d->inum = INT2FIX(1);
	d->scale = 0;
	return WrapDecimal(d);
    }
    if (a == DEC_NaN || l == 1) return x;
    if (a == DEC_PINF || (a != DEC_NINF && a->inum == PZERO)) return x;
    if (a == DEC_NINF || a->inum == NZERO)  {
	if (l % 2 == 0) {
	    return a == DEC_NINF ? VALUE_PINF : dec_uminus(x);
	}
	return x;
    }
    return power_with_fixnum(a, y);
}

static int
normal_cmp(Decimal *x, Decimal *y)
{
    Decimal *max, *min;
    VALUE n;
    const int c = FIX2INT(INUM_CMP(x->inum, y->inum));

    if (x->scale == y->scale) return c;
    if (c == 0) {
	return x->scale > y->scale ? -1 : 1;
    }
    if (c < 0 && x->scale > y->scale) return -1;
    if (c > 0 && x->scale < y->scale) return 1;
    /* XXX: align scales */
    if (x->scale < y->scale) min = x, max = y;
    else min = y, max = x;
    n = inum_lshift(min->inum, max->scale - min->scale);
    if (x == max) return FIX2INT(INUM_CMP(max->inum, n));
    return FIX2INT(INUM_CMP(n, max->inum));
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
 *  Contrast this with +Decimal#eql?+, which requires _other_
 *  to be the same class, a +Decimal+.
 *
 *     Decimal(1) == 1                #=> true
 *     Decimal(1) == Decimal("1.0")   #=> true
 */
static VALUE
dec_eq(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == DEC_NaN) return Qfalse;
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
	    if (b == DEC_NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_cmp(x, y);
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

    GetDecimal(x, a);
    if (a == DEC_NaN) return Qnil;
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
	    if (b == DEC_NaN) return Qnil;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_cmp(x, y);
    }
    return INT2FIX(cmp(a, b));
}

/*
 *  call-seq:
 *     dec > other   => true or false
 *
 *  Returns +true+ if _dec_ is greater than +other+.
 */
static VALUE
dec_gt(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == DEC_NaN) return Qfalse;
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
	    if (b == DEC_NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y);
    }
    return cmp(a, b) > 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec >= other   => true or false
 *
 *  Returns +true+ if _dec_ is greater than or equal to +other+.
 */
static VALUE
dec_ge(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == DEC_NaN) return Qfalse;
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
	    if (b == DEC_NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y);
    }
    return cmp(a, b) >= 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec < other   => true or false
 *
 *  Returns +true+ if _dec_ is less than +other+.
 */
static VALUE
dec_lt(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == DEC_NaN) return Qfalse;
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
	    if (b == DEC_NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y);
    }
    return cmp(a, b) < 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec <= other   => true or false
 *
 *  Returns +true+ if _dec_ is less than or equal to +other+.
 */
static VALUE
dec_le(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == DEC_NaN) return Qfalse;
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
	    if (b == DEC_NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y);
    }
    return cmp(a, b) <= 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec.eql?(other)   => true or false
 *
 *  Returns +true+ if _other_ is a +Decimal+ and is equal to _dec_,
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

    GetDecimal(x, a);
    GetDecimal(y, b);
    if (a == DEC_NaN || b == DEC_NaN) return Qfalse;
    if (DEC_ISINF(a) || DEC_ISINF(b))
	return a == b ? Qtrue : Qfalse;

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
 *   Returns a hash code for _dec_.
 */
static VALUE
dec_hash(VALUE x)
{
    Decimal *d;
    long hash;

    GetDecimal(x, d);
    if (!DEC_IMMEDIATE_P(d)) {
	hash = NUM2LONG(INUM_HASH(d->inum));
	hash ^= d->scale;
    }
    else hash = (long)d;
    return LONG2NUM(hash);
}

/* XXX: any other sane way? */
#define QUIET(stmt) do { \
    RUBY_CRITICAL( \
    const VALUE verbose = ruby_verbose; \
    ruby_verbose = Qnil; \
    stmt; \
    ruby_verbose = verbose; \
    ); \
} while (0)

static double
normal_to_f(Decimal *d)
{
    double f;

    if (d->scale == 0) QUIET(f = NUM2DBL(d->inum));
    else if (d->scale < 0) {
	VALUE n;

	n = inum_lshift(d->inum, -d->scale);
	QUIET(f = NUM2DBL(n));
    }
    else {
	/* FIXME: more strict handling needed for huge value */
	double divf;
	VALUE div;

	QUIET(f = NUM2DBL(d->inum));
	div = inum_lshift(INT2FIX(1), d->scale);
	QUIET(divf = NUM2DBL(div));
	f /= divf;
    }
    if (isinf(f)) {
	rb_warn("Decimal out of Float range");
	f = HUGE_VAL;
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

    GetDecimal(num, d);
    if (d == DEC_NaN)
	f = 0.0 / 0.0;
    else if (d == DEC_PINF)
	f = 1.0 / 0.0;
    else if (d == DEC_NINF)
	f = -1.0 / 0.0;
    else if (d->inum == PZERO)
	f = 0.0;
    else if (d->inum == NZERO)
	f = -0.0;
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
 *     Decimal("34.56").abs    #=> 34.56
 *     Decimal("-34.56").abs   #=> 34.56
 */
static VALUE
dec_abs(VALUE num)
{
    Decimal *d, *d2;

    GetDecimal(num, d);
    if (d == DEC_NINF)
	return VALUE_PINF;
    if (d == DEC_PINF || d == DEC_NaN ||
        d->inum == PZERO || !INUM_NEGATIVE_P(d->inum)) {
	return num;
    }
    d2 = ALLOC(Decimal);
    d2->inum = (d->inum == NZERO) ? PZERO : INUM_UMINUS(d->inum);
    d2->scale = d->scale;
    return WrapDecimal(d2);
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
    do_round(d, 0, ROUND_DOWN, &inum); /* identical to "d.round(0, :down)" */
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
 *  Rounds _dec_ to a given precision in decimal digits (default 0 digits).
 *  Precision may be negative.  Returns a +Decimal+ when _n_ is more than one.
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
	if (!valid_rounding_mode(mode)) {
	    rb_raise(rb_eArgError, "invalid rounding mode %s",
                     RSTRING(rb_inspect(mode))->ptr);
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
    Decimal *d;

    GetDecimal(num, d);
    return d == DEC_NaN ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *     dec.finite?   => true or false
 *
 *  Returns +true+ if _dec_ is a finite number (it is not infinite,
 *  and +nan?+ is +false+).
 *
 *     Decimal(0).finite?             #=> true
 *     Decimal(1).divide(0).finite?   #=> false
 *     Decimal(0).divide(0).finite?   #=> false
 */
static VALUE
dec_finite_p(VALUE num)
{
    Decimal *d;

    GetDecimal(num, d);
    return !DEC_IMMEDIATE_P(d) ? Qtrue : Qfalse;
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
    Decimal *d;

    GetDecimal(num, d);
    if (d == DEC_PINF) return INT2FIX(1);
    if (d == DEC_NINF) return INT2FIX(-1);
    return Qnil;
}

/*
 * +Decimal+ is a decimal fraction that holds exact number in the decimal
 * system unlike +Float+.  It can hold multi-precision digits, so you can
 * calculate any detailed number as you likes.
 */
void
Init_decimal(void)
{
    cDecimal = rb_define_class("Decimal", rb_cNumeric);
    eDomainError = rb_define_class_under(cDecimal, "DomainError",
					 rb_eRangeError);
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
    
#define DEF_ROUNDING_MODE(MODE, mode) do { \
    ROUND_ ## MODE = ID2SYM(rb_intern(#mode)); \
    rb_define_const(cDecimal, "ROUND_" #MODE, ROUND_ ## MODE); \
} while (0)
    DEF_ROUNDING_MODE(CEILING, ceiling);
    DEF_ROUNDING_MODE(DOWN, down);
    DEF_ROUNDING_MODE(FLOOR, floor);
    DEF_ROUNDING_MODE(HALF_DOWN, half_down);
    DEF_ROUNDING_MODE(HALF_EVEN, half_even);
    DEF_ROUNDING_MODE(HALF_UP, half_up);
    DEF_ROUNDING_MODE(UP, up);
    DEF_ROUNDING_MODE(UNNECESSARY, unnecessary);

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
