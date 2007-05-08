#include "ruby.h"
#include "util.h"

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

/*
 * from bignum.c 1.124
 */
#define BDIGITS(x) ((BDIGIT*)RBIGNUM(x)->digits)

#ifndef HAVE_RB_BIG_CMP
static VALUE
rb_big_cmp(VALUE x, VALUE y)
{
    long xlen = RBIGNUM(x)->len;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      default:
	rb_bug("rb_big_cmp(): not reached"); /* modified */
    }

    if (RBIGNUM(x)->sign > RBIGNUM(y)->sign) return INT2FIX(1);
    if (RBIGNUM(x)->sign < RBIGNUM(y)->sign) return INT2FIX(-1);
    if (xlen < RBIGNUM(y)->len)
	return (RBIGNUM(x)->sign) ? INT2FIX(-1) : INT2FIX(1);
    if (xlen > RBIGNUM(y)->len)
	return (RBIGNUM(x)->sign) ? INT2FIX(1) : INT2FIX(-1);

    while(xlen-- && (BDIGITS(x)[xlen]==BDIGITS(y)[xlen]));
    if (-1 == xlen) return INT2FIX(0);
    return (BDIGITS(x)[xlen] > BDIGITS(y)[xlen]) ?
	(RBIGNUM(x)->sign ? INT2FIX(1) : INT2FIX(-1)) :
	    (RBIGNUM(x)->sign ? INT2FIX(-1) : INT2FIX(1));
}
#endif

#ifndef HAVE_RB_BIG_EQ
static VALUE
rb_big_eq(VALUE x, VALUE y)
{
    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;
      case T_BIGNUM:
	break;
      default:
	rb_bug("rb_big_eq(): not reached"); /* modified */
    }
    if (RBIGNUM(x)->sign != RBIGNUM(y)->sign) return Qfalse;
    if (RBIGNUM(x)->len != RBIGNUM(y)->len) return Qfalse;
    if (MEMCMP(BDIGITS(x),BDIGITS(y),BDIGIT,RBIGNUM(y)->len) != 0) return Qfalse;
    return Qtrue;
}
#endif

static VALUE
big_uminus(VALUE x)
{
    VALUE z = rb_big_clone(x);

    RBIGNUM(z)->sign = !RBIGNUM(x)->sign;

    return rb_big_norm(z); /* modified to use exported one */
}

static VALUE
rb_big_hash(VALUE x)
{
    long i, len, key;
    BDIGIT *digits;

    key = 0; digits = BDIGITS(x); len = RBIGNUM(x)->len;
    for (i=0; i<len; i++) {
	key ^= *digits++;
    }
    return LONG2FIX(key);
}

/*
 * from numeric.c 1.129
 */
static VALUE
fix_plus(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long a, b, c;
	VALUE r;

	a = FIX2LONG(x);
	b = FIX2LONG(y);
	c = a + b;
	r = LONG2FIX(c);

	if (FIX2LONG(r) != c) {
	    r = rb_big_plus(rb_int2big(a), rb_int2big(b));
	}
	return r;
    }
    return rb_big_plus(y, x); /* modified */
}

static VALUE
fix_minus(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long a, b, c;
	VALUE r;

	a = FIX2LONG(x);
	b = FIX2LONG(y);
	c = a - b;
	r = LONG2FIX(c);

	if (FIX2LONG(r) != c) {
	    r = rb_big_minus(rb_int2big(a), rb_int2big(b));
	}
	return r;
    }
    return rb_big_minus(rb_int2big(FIX2LONG(x)), y); /* modified */
}

static VALUE
fix_mul(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long a, b, c;
	VALUE r;

	a = FIX2LONG(x);
	if (a == 0) return x;

	b = FIX2LONG(y);
	c = a * b;
	r = LONG2FIX(c);

	if (FIX2LONG(r) != c || c/a != b) {
	    r = rb_big_mul(rb_int2big(a), rb_int2big(b));
	}
	return r;
    }
    return rb_big_mul(y, x); /* modified */
}

static void
fixdivmod(long x, long y, long *divp, long *modp)
{
    long div, mod;

    if (y == 0) rb_bug("fixdivmod(): not reached"); /* modified */
    if (y < 0) {
	if (x < 0)
	    div = -x / -y;
	else
	    div = - (x / -y);
    }
    else {
	if (x < 0)
	    div = - (-x / y);
	else
	    div = x / y;
    }
    mod = x - div*y;
    if ((mod < 0 && y > 0) || (mod > 0 && y < 0)) {
	mod += y;
	div -= 1;
    }
    if (divp) *divp = div;
    if (modp) *modp = mod;
}

static VALUE
fix_div(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long div;

	fixdivmod(FIX2LONG(x), FIX2LONG(y), &div, 0);
	return LONG2NUM(div);
    }
    /* modified */
#ifdef HAVE_RB_BIG_DIV
    return rb_big_div(rb_int2big(FIX2LONG(x), y);
#else
    return RARRAY(rb_big_divmod(rb_int2big(FIX2LONG(x)), y))->ptr[0];
#endif
}

static VALUE
fix_divmod(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long div, mod;

	fixdivmod(FIX2LONG(x), FIX2LONG(y), &div, &mod);

	return rb_assoc_new(LONG2NUM(div), LONG2NUM(mod));
    }
    return rb_big_divmod(rb_int2big(FIX2LONG(x)), y); /* modified */
}

static VALUE
fix_pow(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long a, b;

	b = FIX2LONG(y);
	if (b == 0) return INT2FIX(1);
	if (b == 1) return x;
	a = FIX2LONG(x);
	if (b > 0) {
	    return rb_big_pow(rb_int2big(a), y);
	}
	return rb_float_new(pow((double)a, (double)b));
    }
    return rb_big_pow(rb_int2big(FIX2LONG(x)), y); /* modified */
}

static VALUE
fix_equal(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	return (FIX2LONG(x) == FIX2LONG(y))?Qtrue:Qfalse;
    }
    else {
	return rb_big_eq(y, x); /* modified */
    }
}

static VALUE
fix_cmp(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long a = FIX2LONG(x), b = FIX2LONG(y);

	if (a == b) return INT2FIX(0);
	if (a > b) return INT2FIX(1);
	return INT2FIX(-1);
    }
    else {
	return rb_big_cmp(rb_int2big(FIX2LONG(x)), y); /* modified */
    }
}

/*
 * end of copy
 */


#define INUM_PLUS(a,b) (FIXNUM_P(a)?fix_plus(a,b):rb_big_plus(a,b))
#define INUM_MINUS(a,b) (FIXNUM_P(a)?fix_minus(a,b):rb_big_minus(a,b))
#define INUM_MUL(a,b) (FIXNUM_P(a)?fix_mul(a,b):rb_big_mul(a,b))
#ifdef HAVE_RB_BIG_DIV
#define INUM_DIV(a,b) (FIXNUM_P(a)?fix_div(a,b):rb_big_div(a,b))
#else
#define INUM_DIV(a,b) (FIXNUM_P(a)?fix_div(a,b):RARRAY(rb_big_divmod(a,b))->ptr[0])
#endif
#define INUM_DIVMOD(a,b) (FIXNUM_P(a)?fix_divmod(a,b):rb_big_divmod(a,b))
#define INUM_EQ(a,b) (FIXNUM_P(a)?fix_equal(a,b):rb_big_eq(a,b))
#define INUM_CMP(a,b) (FIXNUM_P(a)?fix_cmp(a,b):rb_big_cmp(a,b))
#define INUM_UMINUS(n) (FIXNUM_P(n)?LONG2NUM(-FIX2LONG(n)):big_uminus(n))
#define INUM_HASH(n) (FIXNUM_P(n)?LONG2NUM((long)n):rb_big_hash(n))
#define INUM2STR(n) (FIXNUM_P(n)?rb_fix2str(n,10):rb_big2str(n,10))

#define INUM_INC(n) do { n=INUM_PLUS(n,INT2FIX(1)); } while (0)
#define INUM_DEC(n) do { n=INUM_MINUS(n,INT2FIX(1)); } while (0)
#define INUM_GT(a,b) (FIX2INT(INUM_CMP(a,b))>0)
#define INUM_LT(a,b) (FIX2INT(INUM_CMP(a,b))<0)
#define INUM_ZERO_P(n) (FIXNUM_P(n)&&FIX2LONG(n)==0)
#define INUM_NEGATIVE_P(n) (FIXNUM_P(n)?FIX2LONG(n)<0:!RBIGNUM(n)->sign)
#ifdef HAVE_RB_BIG_MODULO
#define INUM_BOTTOMDIG(n) (FIXNUM_P(n)?FIX2LONG(n)%10:(RBIGNUM(n)->len?FIX2INT(rb_big_modulo(n,INT2FIX(10))):0))
#else
#define INUM_BOTTOMDIG(n) (FIXNUM_P(n)?FIX2LONG(n)%10:(RBIGNUM(n)->len?FIX2INT(RARRAY(rb_big_divmod(n,INT2FIX(10)))->ptr[1]):0))
#endif
#define INUM_ODD_P(n) (FIXNUM_P(n)?FIX2LONG(n)&1:BDIGITS(n)[0]&1)

typedef struct {
    VALUE inum;
    long scale;
} Decimal;

enum {
    ATTR_PZERO = 1,
    ATTR_NZERO = 2,
    ATTR_PINF = 3,
    ATTR_NINF = 5,
    ATTR_NaN = 6
};

#define ATTR(d) ((unsigned long)(d))
#define ATTR_NORMAL_P(d) ((ATTR(d) & 3) == 0)

static Decimal *const PZERO = (Decimal *)ATTR_PZERO;
static Decimal *const NZERO = (Decimal *)ATTR_NZERO;
static Decimal *const PINF = (Decimal *)ATTR_PINF;
static Decimal *const NINF = (Decimal *)ATTR_NINF;
static Decimal *const NaN = (Decimal *)ATTR_NaN;

#define decimal_pzero() WrapDecimal(PZERO)
#define decimal_nzero() WrapDecimal(NZERO)
#define decimal_pinf() WrapDecimal(PINF)
#define decimal_ninf() WrapDecimal(NINF)
#define decimal_nan() WrapDecimal(NaN)

typedef enum {
    ROUND_CEILING,
    ROUND_DOWN,
    ROUND_FLOOR,
    ROUND_HALF_DOWN,
    ROUND_HALF_EVEN,
    ROUND_HALF_UP,
    ROUND_UP,
    ROUND_UNNECESSARY
} RoundingMode;

#define GetDecimal(obj, d) do {\
    Data_Get_Struct(obj, Decimal, d);\
    if (!d) rb_raise(rb_eArgError, "uninitialized Decimal object");\
} while (0)
#define WrapDecimal(d) Data_Wrap_Struct(cDecimal, decimal_mark, decimal_free, d)
#define DECIMAL_P(d) (RDATA(d)->dmark == decimal_mark)

static VALUE cDecimal;
static VALUE eDomainError;
static VALUE eArithmeticError;

static void
decimal_mark(void *p)
{
    Decimal *d;

    if (!p) return;
    d = (Decimal *)p;
    /* mark if d->inum is Bignum */
    if (ATTR_NORMAL_P(d) && !FIXNUM_P(d->inum)) {
	rb_gc_mark(d->inum);
    }
}

static void
decimal_free(void *p)
{
#ifndef DEBUG
    if (ATTR_NORMAL_P(p)) xfree(p);
#else
    static void *priv = NULL;

    if (ATTR_NORMAL_P(p)) {
	if (p == priv) {
	    fprintf(stderr, "\ndecimal_free(): double-free occured\n");
	}
	xfree(p);
	priv = p;
    }
#endif
}

static Decimal *
inum_to_decimal(VALUE x)
{
    Decimal *d;

    if (INUM_ZERO_P(x)) d = PZERO;
    else {
	d = ALLOC(Decimal);
	d->inum = x;
	d->scale = 0;
    }
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
cstr_to_decimal(const char *str)
#if 0
{
    Decimal *d;
    char *s = strdup(str), *ss, *ss2;
    long scale = 0;
    VALUE inum, assoc[2];

    if (ss = strchr(s, '.')) {
	char *p;

	if (ss == s || !ISDIGIT(ss[-1]) || !ISDIGIT(ss[1])) {
	  bad:
	    xfree(s);
	    rb_invalid_str(str, "Decimal");
	}
	for (p = ss + 1; ISDIGIT(*p) || *p == '_'; p++) {
	    if (ISDIGIT(*p)) scale++;
	}
	memmove(ss, ss+1, strlen(ss)); /* ensure moving '\0' */
    }
    assoc[0] = (VALUE)s;
    assoc[1] = (VALUE)str;
    if (ss2 = strpbrk(s, "eE")) {
	if (ss && ss2 < ss) goto bad;
	*ss2++ = '\0';
	inum = rb_rescue(cstr_to_inum, (VALUE)ss2, invalid_str, (VALUE)assoc);
	scale -= NUM2LONG(inum);
    }
    inum = rb_rescue(cstr_to_inum, (VALUE)s, invalid_str, (VALUE)assoc);
    if (INUM_ZERO_P(inum)) {
	d = strchr(s, '-') ? NZERO : PZERO;
    }
    else {
	d = ALLOC(Decimal);
	d->inum = inum;
	d->scale = scale;
    }
    xfree(s);
    return d;
}
#else
{
    Decimal *d;
    const char *s = strdup(str);
    char *ss, *ss2;
    long scale = 0;
    VALUE inum, assoc[2];

    if (ss = strchr(s, '.')) {
	const char *p;

	*ss = '_'; /* so that rb_cstr_to_inum() can ignore '.' */
	for (p = ss + 1; ; p++) {
	    if (ISDIGIT(*p)) scale++;
	    else if (*p != '_') break;
	}
    }
    assoc[0] = (VALUE)s;   /* as a rb_rescue()'s argument */
    assoc[1] = (VALUE)str;
    if (ss2 = strpbrk(s, "eE")) {
	*ss2++ = '\0'; /* for strchr() */
	inum = rb_rescue(cstr_to_inum, (VALUE)ss2, invalid_str, (VALUE)assoc);
	scale -= NUM2LONG(inum);
    }
    inum = rb_rescue(cstr_to_inum, (VALUE)s, invalid_str, (VALUE)assoc);
    if (INUM_ZERO_P(inum)) d = strchr(s, '-') ? NZERO : PZERO;
    else {
	d = ALLOC(Decimal);
	d->inum = inum;
	d->scale = scale;
    }
    xfree((char *)s);
    return d;
}
#endif /* CONST */

static Decimal *
dup(Decimal *d)
{
    Decimal *d2;

    if (!ATTR_NORMAL_P(d)) return d;
    d2 = ALLOC(Decimal);
    *d2 = *d;
    return d2;
}

static Decimal *
create_decimal(VALUE arg)
{
    switch (TYPE(arg)) {
      case T_FIXNUM:
      case T_BIGNUM:
	return inum_to_decimal(arg);
      case T_STRING:
	return cstr_to_decimal(StringValueCStr(arg));
      case T_DATA:
	if (DECIMAL_P(arg)) {
	    Decimal *d;

	    GetDecimal(arg, d);
	    return dup(d);
	}
	/* fall through */
      default:
	rb_raise(rb_eArgError, "invalid value for Decimal: %s",
                 RSTRING(rb_inspect(arg))->ptr);
    }
    return NULL; /* not reached */
}

static VALUE
f_decimal(VALUE obj, VALUE arg)
{
    if (TYPE(arg) == T_DATA && DECIMAL_P(arg)) return arg;
    return WrapDecimal(create_decimal(arg));
}

static VALUE
decimal_s_allocate(VALUE klass)
{
    return Data_Wrap_Struct(klass, decimal_mark, decimal_free, 0);
}

static VALUE
decimal_initialize(VALUE self, VALUE arg)
{
    DATA_PTR(self) = create_decimal(arg);
    return self;
}

static VALUE
decimal_induced_from(VALUE klass, VALUE x)
{
    switch (TYPE(x)) {
      case T_FIXNUM:
      case T_BIGNUM:
	return WrapDecimal(inum_to_decimal(x));
      case T_FLOAT:
        {
	    double f = RFLOAT(x)->value;
	    char buf[DBL_DIG+9]; /* 9 == ("-" "." "e-1234" "\0").length */

	    if (isinf(f)) return f > 0 ? decimal_pinf() : decimal_ninf();
	    if (isnan(f)) return decimal_nan();
	    if (f == 0) {
	    /* XXX: Is there smarter way to tell "-0.0" from "0.0"? */
		if (1 / f > 0) return decimal_pzero();
		return decimal_nzero();
	    }
	    sprintf(buf, "%.*g", DBL_DIG, f); /* f is normal */
	    return WrapDecimal(cstr_to_decimal(buf));
        }
      case T_DATA:
	if (DECIMAL_P(x)) return x;
	/* fall through */
      default:
	rb_raise(rb_eTypeError, "failed to convert %s into Decimal",
		 rb_obj_classname(x));
    }
    return Qnil; /* not reached */
}

#ifdef DEBUG
static VALUE
decimal_scale(VALUE self)
{
    Decimal *d;

    GetDecimal(self, d); 
    if (ATTR_NORMAL_P(d))
	return LONG2NUM(d->scale);
    if (d == PZERO || d == NZERO)
	return INT2FIX(0);
    return Qnil;
}

static VALUE
decimal_unscaled_value(self)
    VALUE self;
{
    Decimal *d;

    GetDecimal(self, d);
    return ATTR_NORMAL_P(d) ? d->inum : Qnil;
}
#endif

static VALUE
decimal_strip_trailing_zeros(VALUE self)
{
    Decimal *d, *d2;

    GetDecimal(self, d);
    if (!ATTR_NORMAL_P(d))
	return self;
    d2 = ALLOC(Decimal);
    *d2 = *d;
    /* XXX: can be optimized when inum is Bignum */
    while (INUM_BOTTOMDIG(d2->inum) == 0) {
	d2->inum = INUM_DIV(d2->inum, INT2FIX(10));
	d2->scale--;
    }
    return WrapDecimal(d2);
}

/* XXX: should return "%g" formatted string? */
static VALUE
normal_to_s(Decimal *d)
{
    const VALUE str = INUM2STR(d->inum);
    const char *s = RSTRING(str)->ptr;
    long slen = RSTRING(str)->len, snumlen, sslen;
    int negative;
    long scale = d->scale;
    char *ss;
    VALUE newstr;

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
	long diff = slen - scale;

	sslen = slen + 1;
	ss = ALLOC_N(char, sslen);
	memcpy(ss, s, diff);
	ss[diff] = '.';
	memcpy(ss+diff+1, s+diff, scale);
    }
    else { /* "0.00xx" */
	long diff = scale - snumlen;
	char *ss2;

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
decimal_to_s(VALUE self)
{
    Decimal *d;

    GetDecimal(self, d);
    switch (ATTR(d)) {
      case ATTR_PZERO:
	return rb_str_new2("0");
      case ATTR_NZERO:
	return rb_str_new2("-0");
      case ATTR_PINF:
	return rb_str_new2("Infinity");
      case ATTR_NINF:
	return rb_str_new2("-Infinity");
      case ATTR_NaN:
	return rb_str_new2("NaN");
      default:
	if (!ATTR_NORMAL_P(d)) {
	    rb_bug("to_s: illegal attribute %lu",  ATTR(d));
	}
	return normal_to_s(d);
    }
    return Qnil; /* not reached */
}

static VALUE
decimal_inspect(VALUE self)
{
    char *s;
    VALUE str, newstr;

    str = decimal_to_s(self);
    /* strlen("Decimal()") == 9 */
    s = ALLOC_N(char, 9 + RSTRING(str)->len + 1);
    sprintf(s, "Decimal(%s)", RSTRING(str)->ptr);
    newstr = rb_str_new(s, 9 + RSTRING(str)->len);
    xfree(s);
    return newstr;
}

static VALUE
decimal_coerce(VALUE x, VALUE y)
{
    VALUE d;

    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	d = WrapDecimal(inum_to_decimal(y));
	return rb_assoc_new(d, x);
      case T_DATA:
	if (DECIMAL_P(y)) return rb_assoc_new(y, x);
	/* fall through */
      default:
	rb_raise(rb_eTypeError, "can't coerce %s to Decimal",
		 rb_obj_classname(y));
    }
    return Qnil; /* not reached */
}

static VALUE
decimal_uminus(VALUE num)
{
    Decimal *d, *d2;

    GetDecimal(num, d);
    switch (ATTR(d)) {
      case ATTR_PZERO:
	return decimal_nzero();
      case ATTR_NZERO:
	return decimal_pzero();
      case ATTR_PINF:
	return decimal_ninf();
      case ATTR_NINF:
	return decimal_pinf();
      case ATTR_NaN:
	return num;
      default:
	if (!ATTR_NORMAL_P(d)) {
	    rb_bug("-@: illegal attribute %lu",  ATTR(d));
	}
	d2 = ALLOC(Decimal);
	d2->inum = INUM_UMINUS(d->inum);
	d2->scale = d->scale;
	return WrapDecimal(d2);
    }
    return Qnil; /* not reached */
}

static VALUE
inum_shift(VALUE x, long n)
{
    VALUE y;

    if (n < 0) rb_bug("inum_shift(): not reached");
    if (n == 0) return x;
    y = fix_pow(INT2FIX(10), LONG2NUM(n));
    return INUM_MUL(x, y);
}

static Decimal *
normal_plus(Decimal *x, Decimal *y, const int sign)
{
    Decimal *z;
    VALUE inum;
    long scale;

    if (x->scale == y->scale) {
	inum = sign ? INUM_PLUS(x->inum, y->inum) : INUM_MINUS(x->inum, y->inum);
	scale = x->scale;
    }
    else {
	long diff;
        VALUE xnum, ynum;

	if (x->scale < y->scale) {
            scale = y->scale;
            diff = y->scale - x->scale;
            xnum = inum_shift(x->inum, diff);
            ynum = y->inum;
        }
        else {
            scale = x->scale;
            diff = x->scale - y->scale;
            ynum = inum_shift(y->inum, diff);
            xnum = x->inum;
        }
	inum = sign ? INUM_PLUS(xnum, ynum) : INUM_MINUS(xnum, ynum);
    }
    if (INUM_ZERO_P(inum)) z = PZERO;
    else {
	z = ALLOC(Decimal);
	z->inum = inum;
	z->scale = scale;
    }
    return z;
}

static VALUE
decimal_plus(VALUE x, VALUE y)
{
    Decimal *a, *b;

    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return decimal_nan();
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y);
    }
    GetDecimal(x, a);
    if (a == NaN) return decimal_nan();
    switch (ATTR(a)) {
      case ATTR_PZERO:
	if (b == NZERO) return x;
	/* fall through */
      case ATTR_NZERO:
	return y;
      case ATTR_PINF:
	if (b == NINF) return decimal_nan();
	return x;
      case ATTR_NINF:
	if (b == PINF) return decimal_nan();
	return x;
      default:
	if (!ATTR_NORMAL_P(a)) {
	    rb_bug("+: illegal attribute %lu",  ATTR(a));
	}
	if (b == PZERO || b == NZERO) return x;
	if (b == PINF || b == NINF) return y;
	return WrapDecimal(normal_plus(a, b, 1));
    }
    return Qnil; /* not reached */
}

static VALUE
decimal_minus(VALUE x, VALUE y)
{
    Decimal *a, *b;

    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return decimal_nan();
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y);
    }
    GetDecimal(x, a);
    if (a == NaN) return decimal_nan();
    switch (ATTR(a)) {
      case ATTR_PZERO:
	if (b == PZERO) return x;
	/* fall through */
      case ATTR_NZERO:
	return decimal_uminus(y);
      case ATTR_PINF:
      case ATTR_NINF:
	if (a == b) return decimal_nan();
	return x;
      default:
	if (!ATTR_NORMAL_P(a)) {
	    rb_bug("-: illegal attribute %lu",  ATTR(a));
	}
	if (b == PZERO || b == NZERO) return x;
	if (b == PINF || b == NINF) return decimal_uminus(y);
	return WrapDecimal(normal_plus(a, b, 0));
    }
    return Qnil; /* not reached */
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
decimal_mul(VALUE x, VALUE y)
{
    Decimal *a, *b;

    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return decimal_nan();
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y);
    }
    GetDecimal(x, a);
    if (a == NaN) return decimal_nan();
    switch (ATTR(a)) {
      case ATTR_PZERO:
	if (ATTR_NORMAL_P(b)) {
	    return INUM_NEGATIVE_P(b->inum) ? decimal_nzero() : x;
	}
	if (b == PZERO || b == NZERO) return y;
	return decimal_nan();
      case ATTR_NZERO:
	if (ATTR_NORMAL_P(b)) {
	    return INUM_NEGATIVE_P(b->inum) ? decimal_pzero() : x;
	}
	if (b == PZERO || b == NZERO) {
	    return decimal_uminus(y);
	}
	return decimal_nan();
      case ATTR_PINF:
	if (ATTR_NORMAL_P(b)) {
	    return INUM_NEGATIVE_P(b->inum) ? decimal_ninf() : x;
	}
	if (b == PINF || b == NINF) return y;
	return decimal_nan();
      case ATTR_NINF:
	if (ATTR_NORMAL_P(b)) {
	    return INUM_NEGATIVE_P(b->inum) ? decimal_pinf() : x;
	}
	if (b == PINF || b == NINF) {
	    return decimal_uminus(y);
	}
	return decimal_nan();
      default:
	if (!ATTR_NORMAL_P(a)) {
	    rb_bug("*: illegal attribute %lu",  ATTR(a));
	}
	if (ATTR_NORMAL_P(b)) return WrapDecimal(normal_mul(a, b));
	if (INUM_NEGATIVE_P(a->inum)) return decimal_uminus(y);
	return y;
    }
    return Qnil; /* not reached */
}

static Decimal *
do_round(Decimal *d, long scale, RoundingMode mode, VALUE *inump)
{
    Decimal *d2;
    long diff;
    int lower;
    int trailing_nonzero, negative;
    VALUE inum, inumabs, shift, tmp, ary;

    switch (ATTR(d)) {
      case ATTR_PZERO:
      case ATTR_NZERO:
	if (scale == 0 && inump) {
	    *inump = INT2FIX(0);
	    return NULL;
	}
	return d;
      case ATTR_PINF:
	rb_raise(eDomainError, "Infinity");
      case ATTR_NINF:
	rb_raise(eDomainError, "-Infinity");
      case ATTR_NaN:
	rb_raise(eDomainError, "NaN");
      default:
	if (!ATTR_NORMAL_P(d)) {
	    rb_bug("do_round(): illegal attribute %lu",  ATTR(d));
	}
	break;
    }
    if (d->scale <= scale) { /* no need to round */
	/* return Decimal */
	if (scale) return dup(d);
	/* return Integer */
	/* FIXME: why above is not needed?
	 * if (!inump) rb_bug("do_round(): not reached");
	 */
	if (d->scale == 0) *inump = d->inum;
	else *inump = inum_shift(d->inum, -d->scale);
	return NULL;
    }
    negative = INUM_NEGATIVE_P(d->inum);
    diff = d->scale - scale;
    inumabs = negative ? INUM_UMINUS(d->inum) : d->inum;
    switch (mode) {
      case ROUND_CEILING:
      case ROUND_DOWN:
      case ROUND_FLOOR:
      case ROUND_UP:
      case ROUND_UNNECESSARY:
	shift = inum_shift(INT2FIX(1), diff);
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
	break;
      case ROUND_HALF_DOWN: /* below modes need lower digit */
      case ROUND_HALF_UP:
      case ROUND_HALF_EVEN:
	shift = inum_shift(INT2FIX(1), diff-1);
	ary = INUM_DIVMOD(inumabs, shift);
	tmp = RARRAY(ary)->ptr[0];

	ary = INUM_DIVMOD(tmp, INT2FIX(10));
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
	break;
      default:
	rb_bug("do_round(): illegal rounding mode %d",  mode);
    }
  coda:
    if (negative) inum = INUM_UMINUS(inum);
    if (scale == 0 && inump) { /* return Integer */
	*inump = inum;
	return NULL;
    }
    /* return Decimal */
    if (INUM_ZERO_P(inum)) {
	d2 = negative ? NZERO : PZERO;
    }
    else {
	d2 = ALLOC(Decimal);
	d2->scale = scale;
	d2->inum = inum;
    }
    return d2;
}

static Decimal *
normal_divide(Decimal *x, Decimal *y, long scale, RoundingMode mode)
{
    long diff;
    VALUE xx;
    Decimal *z = ALLOC(Decimal);

    diff = x->scale - y->scale;
    if (diff <= scale) {
	xx = inum_shift(x->inum, scale-diff+1); /* +1 for rounding */
	z->scale = scale + 1;
    }
    else {
	/* XXX: may be a bug...? */
	xx = x->inum;
	z->scale = diff;
    }
    z->inum = INUM_DIV(xx, y->inum);
    return do_round(z, scale, mode, 0);
}

static VALUE
decimal_divide(int argc, VALUE *argv, VALUE x)
{
    VALUE y;
    Decimal *a, *b;
    /*
     * XXX: The default rounding mode is decided to be consistent with
     *      Integer#/ behavior; is this the right way?
     */
    RoundingMode mode = ROUND_FLOOR;
    long scale = 0; /* comfort compiler :-( */
    int use_default_scale = Qtrue;
    VALUE vscale, vmode;

    GetDecimal(x, a);
    rb_scan_args(argc, argv, "12", &y, &vscale, &vmode);
    switch (argc) {
      case 3:
	mode = NUM2INT(vmode);
	if (mode < 0 || mode > ROUND_UNNECESSARY) {
	    rb_raise(rb_eArgError, "illegal rounding mode %d", mode);
	}
	/* fall through */
      case 2:
	scale = NUM2LONG(vscale);
	if (scale < 0) {
	    rb_raise(rb_eArgError, "negative argument %ld", scale);
	}
	use_default_scale = Qfalse;
	/* fall through */
      default:
	break;
    }
    switch (TYPE(y)) {
      case T_FIXNUM:
	/* XXX: can be optimized if y == 1, -1, or 0 */
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	rb_raise(rb_eTypeError, "can't operate with Float");
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return decimal_nan();
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_bin(x, y);
    }
    if (a == NaN) return decimal_nan();
    switch (ATTR(a)) {
      case ATTR_PZERO:
	if (ATTR_NORMAL_P(b)) {
	    return INUM_NEGATIVE_P(b->inum) ? decimal_nzero() : decimal_pzero();
	}
	if (b == PZERO || b == NZERO) {
	    return decimal_nan();
	}
	if (b == PINF) return x;
	return decimal_nzero(); /* b == NINF */
      case ATTR_NZERO:
	if (ATTR_NORMAL_P(b)) {
	    return INUM_NEGATIVE_P(b->inum) ? decimal_pzero() : decimal_nzero();
	}
	if (b == PZERO || b == NZERO) return decimal_nan();
	if (b == PINF) return x;
	return decimal_pzero(); /* b == NINF */
      case ATTR_PINF:
	if (ATTR_NORMAL_P(b)) {
	    return INUM_NEGATIVE_P(b->inum) ? decimal_ninf() : x;
	}
	if (b == PZERO) return x;
	if (b == NZERO) return decimal_ninf();
	return decimal_nan();
      case ATTR_NINF:
	if (ATTR_NORMAL_P(b)) {
	    return INUM_NEGATIVE_P(b->inum) ? decimal_pinf() : x;
	}
	if (b == PZERO) return x;
	if (b == NZERO) return decimal_pinf();
	return decimal_nan();
      default:
	if (!ATTR_NORMAL_P(a)) {
	    rb_bug("divide: illegal attribute %lu",  ATTR(a));
	}
	if (use_default_scale) scale = a->scale;
	if (ATTR_NORMAL_P(b)) {
	    return WrapDecimal(normal_divide(a, b, scale, mode));
	}
	if (b == PZERO) {
	    return INUM_NEGATIVE_P(a->inum) ? decimal_ninf() : decimal_pinf();
	}
	if (b == NZERO) {
	    return INUM_NEGATIVE_P(a->inum) ? decimal_pinf() : decimal_ninf();
	}
	if (b == PINF) {
	    return INUM_NEGATIVE_P(a->inum) ? decimal_nzero() : decimal_pzero();
	}
	/* b == NINF */
	return INUM_NEGATIVE_P(a->inum) ? decimal_pzero() : decimal_nzero();
    }
    return Qnil; /* not reached */
}

static VALUE
decimal_div(VALUE x, VALUE y)
{
    return decimal_divide(1, &y, x);
}

/*
 * FIXME: test needed!
 */
static void
divmod(Decimal *a, Decimal *b, VALUE *divp, VALUE *modp)
{
    Decimal *div, *mod;

    if (a == NaN || b == NaN ||	b == PZERO || b == NZERO ||
	a == PINF || a == NINF) {
	div = mod = NaN;
    }
    else if (a == PZERO || a == NZERO) {
	const int b_negative = INUM_NEGATIVE_P(b->inum);

	if (b == PINF || (ATTR_NORMAL_P(b) && !b_negative)) {
	    div = PZERO;
	}
	else if (b == NINF || (ATTR_NORMAL_P(b) && b_negative)) {
	    div = NZERO;
	}
	else rb_bug("divmod(): not reached");
	mod = a;
    }
    else if (ATTR_NORMAL_P(a) && (b == PINF || b == NINF)) {
	const int a_negative = INUM_NEGATIVE_P(a->inum);

	if ((a_negative && b == PINF) || (!a_negative && b == NINF)) {
	    div = ALLOC(Decimal);
	    div->inum = INT2FIX(-1);
	    div->scale = 0;
	    mod = b;
	}
	else {
	    if (a_negative) div = NZERO;
	    else div = PZERO;
	    mod = dup(a);
	}
    }
    else if (ATTR_NORMAL_P(a) && ATTR_NORMAL_P(b)) {
	const int b_negative = INUM_NEGATIVE_P(b->inum);
	int mod_negative;

	div = normal_divide(a, b, 0, ROUND_DOWN); /* div = x / y */
	if (div == PZERO || div == NZERO) {
	    if (div == PZERO && b_negative) div = NZERO;
	    mod = dup(a);
	}
	else if (ATTR_NORMAL_P(div)) {
	    Decimal *tmpd;

	    div = dup(div);
	    tmpd = normal_mul(div, b);
	    mod = normal_plus(a, tmpd, 0); /*  mod = x - div*y; */
	    xfree(tmpd);
	}
	else rb_bug("divmod(): illegal attribute %lu in div", ATTR(div));
	/* if ((mod < 0 && y > 0) || (mod > 0 && y < 0)) { */
	if (ATTR_NORMAL_P(mod) &&
	    (((mod_negative=INUM_NEGATIVE_P(mod->inum)) && !b_negative) ||
	     (!mod_negative && b_negative))) {
	    Decimal *tmp;

	    tmp = normal_plus(mod, b, 1); /*  mod += y; */
	    xfree(mod);
	    mod = tmp;
	    if (ATTR_NORMAL_P(div)) INUM_DEC(div->inum); /* div -= 1; */
	    else if (div == PZERO || div == NZERO) {
		div = ALLOC(Decimal);
		div->inum = INT2FIX(-1);
		div->scale = 0;
	    }
	    else rb_bug("divmod(): illegal attribute %lu in div[2]", ATTR(div));
	}
    }
    else rb_bug("divmod(): not reached[2]");

    if (divp) *divp = WrapDecimal(div);
    else if (ATTR_NORMAL_P(div)) xfree(div);
    if (modp) *modp = WrapDecimal(mod);
    else  if (ATTR_NORMAL_P(mod)) xfree(mod);
}

static VALUE
decimal_idiv(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE div;

    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
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
    divmod(a, b, &div, 0);
    return div;
}

static VALUE
decimal_mod(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE mod;

    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
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
    divmod(a, b, 0, &mod);
    return mod;
}

static VALUE
decimal_divmod(VALUE x, VALUE y)
{
    Decimal *a, *b;
    VALUE div, mod;

    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
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
power_with_long(Decimal *x, long y)
{
    Decimal *z = x, *tmp;
    Decimal *const orig_x = x;

    if (y <= 1) rb_bug("power_with_long(): not reached");
    for (;;) {
	y -= 1;
	if (y == 0) break;
	while (y % 2 == 0) {
	    y /= 2;
	    tmp = normal_mul(x, x);
	    if (!FIXNUM_P(tmp->inum)) rb_gc_mark(tmp->inum);
	    if (x != orig_x) xfree(x);
	    x = tmp;
	}
	tmp = normal_mul(z, x);
	if (!FIXNUM_P(tmp->inum)) rb_gc_mark(tmp->inum);
	if (z != orig_x) xfree(z);
	z = tmp;
    }
    return WrapDecimal(z);
}

static VALUE
decimal_pow(VALUE x, VALUE y)
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
    if (a == NaN || l == 1) return x;
    switch (ATTR(a)) {
      case ATTR_PZERO:
	return x;
      case ATTR_NZERO:
	return l % 2 == 0 ? decimal_pzero() : x;
      case ATTR_PINF:
	return x;
      case ATTR_NINF:
	return l % 2 == 0 ? decimal_pinf() : x;
      default:
	if (!ATTR_NORMAL_P(a)) {
	    rb_bug("**: illegal attribute %lu",  ATTR(a));
	}
	return power_with_long(a, l);
    }
    return Qnil; /* not reached */
}

static int
normal_cmp(Decimal *x, Decimal *y)
{
    Decimal *max, *min;
    long diff;
    VALUE n;

    if (x->scale == y->scale) return FIX2INT(INUM_CMP(x->inum, y->inum));
    if (INUM_EQ(x->inum, y->inum)) {
	return x->scale > y->scale ? -1 :
	       x->scale < y->scale ? 1 :
	       0;
    }
    if (INUM_GT(x->inum, y->inum) && x->scale < y->scale) return 1;
    if (INUM_LT(x->inum, y->inum) && x->scale > y->scale) return -1;
    if (x->scale < y->scale) min = x, max = y;
    else min = y, max = x;
    diff = max->scale - min->scale;
    n = inum_shift(min->inum, diff);
    if (x == max) return FIX2INT(INUM_CMP(max->inum, n));
    return FIX2INT(INUM_CMP(n, max->inum));
}

static int
cmp(Decimal *x, Decimal *y)
{
    switch (ATTR(x)) {
      case ATTR_PZERO:
      case ATTR_NZERO:
	if (ATTR_NORMAL_P(y)) return INUM_NEGATIVE_P(y->inum) ? 1 : -1;
	if (y == NINF) return 1;
	if (y == PINF) return -1;
	return 0;
      case ATTR_PINF:
	if (y == PINF) return 0;
	return 1;
      case ATTR_NINF:
	if (y == NINF) return 0;
	return -1;
      default:
	if (!ATTR_NORMAL_P(x)) {
	    rb_bug("cmp(): illegal attribute %lu",  ATTR(x));
	}
	if (ATTR_NORMAL_P(y)) return normal_cmp(x, y);
	if (y == PINF) return -1;
	if (y == NINF) return 1;
	return INUM_NEGATIVE_P(x->inum) ? -1 : 1;
    }
    return 0; /* not reached */
}

static VALUE
decimal_eq(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == NaN) return Qfalse;
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	return Qnil;
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_cmp(x, y);
    }
    return cmp(a, b) == 0 ? Qtrue : Qfalse;
}

static VALUE
decimal_cmp(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == NaN) return Qnil;
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	return Qnil;
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return Qnil;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_cmp(x, y);
    }
    return INT2FIX(cmp(a, b));
}

static VALUE
decimal_gt(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == NaN) return Qfalse;
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	rb_cmperr(x, y);
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y);
    }
    return cmp(a, b) > 0 ? Qtrue : Qfalse;
}

static VALUE
decimal_ge(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == NaN) return Qfalse;
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	rb_cmperr(x, y);
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y);
    }
    return cmp(a, b) >= 0 ? Qtrue : Qfalse;
}

static VALUE
decimal_lt(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == NaN) return Qfalse;
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	rb_cmperr(x, y);
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y);
    }
    return cmp(a, b) < 0 ? Qtrue : Qfalse;
}

static VALUE
decimal_le(VALUE x, VALUE y)
{
    Decimal *a, *b;

    GetDecimal(x, a);
    if (a == NaN) return Qfalse;
    switch (TYPE(y)) {
      case T_FIXNUM:
      case T_BIGNUM:
	b = inum_to_decimal(y);
	break;
      case T_FLOAT:
	rb_cmperr(x, y);
      case T_DATA:
	if (DECIMAL_P(y)) {
	    GetDecimal(y, b);
	    if (b == NaN) return Qfalse;
	    break;
	}
	/* fall through */
      default:
	return rb_num_coerce_relop(x, y);
    }
    return cmp(a, b) <= 0 ? Qtrue : Qfalse;
}

static VALUE
decimal_eql(VALUE x, VALUE y)
{
    if (TYPE(y) == T_DATA && DECIMAL_P(y)) {
	Decimal *a, *b;

	GetDecimal(x, a);
	GetDecimal(y, b);
	if (a == NaN || b == NaN) return Qfalse;
	if (ATTR_NORMAL_P(a) && ATTR_NORMAL_P(b)) {
	    return a->scale == b->scale && INUM_EQ(a->inum, b->inum) ?
		Qtrue : Qfalse;
	}
	return a == b ? Qtrue : Qfalse;
    }
    return Qfalse;
}

static VALUE
decimal_hash(VALUE x)
{
    Decimal *d;
    long hash;

    GetDecimal(x, d);
    if (ATTR_NORMAL_P(d)) {
	hash = NUM2LONG(INUM_HASH(d->inum));
	hash += d->scale;
    }
    else hash = ATTR(d);
    return LONG2NUM(hash);
}

#define QUIET(stmt) do {\
    VALUE verbose = ruby_verbose;\
    ruby_verbose = Qnil;\
    stmt;\
    ruby_verbose = verbose;\
} while (0)

static double
normal_to_f(Decimal *d)
{
    double f;

    if (d->scale == 0) QUIET(f = NUM2DBL(d->inum));
    else if (d->scale < 0) {
	VALUE n;

	n = inum_shift(d->inum, -d->scale);
	QUIET(f = NUM2DBL(n));
    }
    else {
	/* FIXME: more strict handling is needed for huge value */
	double divf;
	VALUE div;

	QUIET(f = NUM2DBL(d->inum));
	div = inum_shift(INT2FIX(1), d->scale);
	QUIET(divf = NUM2DBL(div));
	f /= divf;
    }
    if (isinf(f)) {
	rb_warn("Decimal out of Float range");
	f = HUGE_VAL;
    }
    return f;
}

static VALUE
decimal_to_f(VALUE num)
{
    Decimal *d;
    double f;

    GetDecimal(num, d);
    switch (ATTR(d)) {
      case ATTR_PZERO:
	f = 0.0;
	break;
      case ATTR_NZERO:
	f = -0.0;
	break;
      case ATTR_PINF:
	f = 1.0 / 0.0;
	break;
      case ATTR_NINF:
	f = -1.0 / 0.0;
	break;
      case ATTR_NaN:
	f = 0.0 / 0.0;
	break;
      default:
	if (!ATTR_NORMAL_P(d)) {
	    rb_bug("to_f: illegal attribute %lu",  ATTR(d));
	}
	f = normal_to_f(d);
	break;
    }
    return rb_float_new(f);
}

static VALUE
decimal_abs(VALUE num)
{
    Decimal *d, *d2;

    GetDecimal(num, d);
    switch (ATTR(d)) {
      case ATTR_PZERO:
      case ATTR_PINF:
      case ATTR_NaN:
	return num;
      case ATTR_NZERO:
	return decimal_pzero();
      case ATTR_NINF:
	return decimal_pinf();
      default:
	if (!ATTR_NORMAL_P(d)) {
	    rb_bug("abs: illegal attribute %lu",  ATTR(d));
	}
	break;
    }
    if (!INUM_NEGATIVE_P(d->inum)) return num;
    d2 = ALLOC(Decimal);
    d2->inum = INUM_UMINUS(d->inum);
    d2->scale = d->scale;
    return WrapDecimal(d2);
}

static VALUE
decimal_zero_p(VALUE num)
{
    Decimal *d;

    GetDecimal(num, d);
    return d == PZERO || d == NZERO ? Qtrue : Qfalse;
}

static VALUE
decimal_to_i(VALUE num)
{
    Decimal *d;
    VALUE inum;

    GetDecimal(num, d);
    do_round(d, 0, ROUND_DOWN, &inum);
    return inum;
}

static VALUE
rounding_method(int argc, VALUE *argv, VALUE x, RoundingMode mode)
{
    Decimal *d;
    VALUE vscale;
    long scale;

    rb_scan_args(argc, argv, "01", &vscale);
    if (argc == 0) scale = 0;
    else {
	scale = NUM2LONG(vscale);
	if (scale < 0) {
	    rb_raise(rb_eArgError, "negative argument %ld", scale);
	}
    }
    GetDecimal(x, d);
    if (scale == 0) {
	VALUE inum;

	do_round(d, scale, mode, &inum);
	return inum;
    }
    return WrapDecimal(do_round(d, scale, mode, 0));
}

static VALUE
decimal_truncate(int argc, VALUE *argv, VALUE x)
{
    return rounding_method(argc, argv, x, ROUND_DOWN);
}

static VALUE
decimal_floor(int argc, VALUE *argv, VALUE x)
{
    return rounding_method(argc, argv, x, ROUND_FLOOR);
}

static VALUE
decimal_ceil(int argc, VALUE *argv, VALUE x)
{
    return rounding_method(argc, argv, x, ROUND_CEILING);
}

static VALUE
decimal_round(int argc, VALUE *argv, VALUE x)
{
    Decimal *d;
    VALUE vscale, vmode;
    long scale = 0;
    RoundingMode mode = ROUND_HALF_UP;

    rb_scan_args(argc, argv, "02", &vscale, &vmode);
    switch (argc) {
      case 2:
	mode = NUM2INT(vmode);
	if (mode < 0 || mode > ROUND_UNNECESSARY) {
	    rb_raise(rb_eArgError, "illegal rounding mode %d", mode);
	}
	/* fall through */
      case 1:
	scale = NUM2LONG(vscale);
	if (scale < 0) {
	    rb_raise(rb_eArgError, "negative argument %ld", scale);
	}
	/* fall through */
      default:
	break;
    }
    GetDecimal(x, d);
    if (scale == 0) {
	VALUE inum;

	do_round(d, scale, mode, &inum);
	return inum;
    }
    return WrapDecimal(do_round(d, scale, mode, 0));
}


static VALUE
decimal_is_nan_p(VALUE num)
{
    Decimal *d;

    GetDecimal(num, d);
    return d == NaN ? Qtrue : Qfalse;
}

static VALUE
decimal_is_finite_p(VALUE num)
{
    Decimal *d;

    GetDecimal(num, d);
    return d == PZERO || d == NZERO || ATTR_NORMAL_P(d) ? Qtrue : Qfalse;
}

static VALUE
decimal_is_infinite_p(VALUE num)
{
    Decimal *d;

    GetDecimal(num, d);
    if (d == PINF) return INT2FIX(1);
    if (d == NINF) return INT2FIX(-1);
    return Qnil;
}

void
Init_decimal(void)
{
    cDecimal = rb_define_class("Decimal", rb_cNumeric);
    rb_include_module(cDecimal, rb_mPrecision);
    eDomainError = rb_define_class_under(cDecimal, "DomainError", rb_eRangeError);
    eArithmeticError = rb_define_class_under(cDecimal, "ArithmeticError", rb_eStandardError);

    rb_define_global_function("Decimal", f_decimal, 1);

    rb_define_alloc_func(cDecimal, decimal_s_allocate);
    rb_define_method(cDecimal, "initialize", decimal_initialize, 1);
    rb_define_singleton_method(cDecimal, "induced_from", decimal_induced_from, 1);

    rb_define_const(cDecimal, "ROUND_CEILING", INT2FIX(ROUND_CEILING));
    rb_define_const(cDecimal, "ROUND_DOWN", INT2FIX(ROUND_DOWN));
    rb_define_const(cDecimal, "ROUND_FLOOR", INT2FIX(ROUND_FLOOR));
    rb_define_const(cDecimal, "ROUND_HALF_DOWN", INT2FIX(ROUND_HALF_DOWN));
    rb_define_const(cDecimal, "ROUND_HALF_EVEN", INT2FIX(ROUND_HALF_EVEN));
    rb_define_const(cDecimal, "ROUND_HALF_UP", INT2FIX(ROUND_HALF_UP));
    rb_define_const(cDecimal, "ROUND_UP", INT2FIX(ROUND_UP));
    rb_define_const(cDecimal, "ROUND_UNNECESSARY", INT2FIX(ROUND_UNNECESSARY));

#ifdef DEBUG
    rb_define_method(cDecimal, "scale", decimal_scale, 0);
    rb_define_method(cDecimal, "unscaled_value", decimal_unscaled_value, 0);
#endif
    rb_define_method(cDecimal, "strip_trailing_zeros", decimal_strip_trailing_zeros, 0);
    rb_define_method(cDecimal, "strip", decimal_strip_trailing_zeros, 0);
    
    rb_define_method(cDecimal, "to_s", decimal_to_s, 0);
    rb_define_method(cDecimal, "inspect", decimal_inspect, 0);
    rb_define_method(cDecimal, "coerce", decimal_coerce, 1);
    rb_define_method(cDecimal, "-@", decimal_uminus, 0);
    rb_define_method(cDecimal, "+", decimal_plus, 1);
    rb_define_method(cDecimal, "-", decimal_minus, 1);
    rb_define_method(cDecimal, "*", decimal_mul, 1);
    rb_define_method(cDecimal, "divide", decimal_divide, -1);
    rb_define_method(cDecimal, "/", decimal_div, 1);
    rb_define_method(cDecimal, "div", decimal_idiv, 1);
    rb_define_method(cDecimal, "%", decimal_mod, 1);
    rb_define_method(cDecimal, "modulo", decimal_mod, 1);
    rb_define_method(cDecimal, "divmod", decimal_divmod, 1);
    rb_define_method(cDecimal, "**", decimal_pow, 1);
    rb_define_method(cDecimal, "==", decimal_eq, 1);
    rb_define_method(cDecimal, "<=>", decimal_cmp, 1);
    rb_define_method(cDecimal, ">", decimal_gt, 1);
    rb_define_method(cDecimal, ">=", decimal_ge, 1);
    rb_define_method(cDecimal, "<", decimal_lt, 1);
    rb_define_method(cDecimal, "<=", decimal_le, 1);
    rb_define_method(cDecimal, "eql?", decimal_eql, 1);
    rb_define_method(cDecimal, "hash", decimal_hash, 0);
    rb_define_method(cDecimal, "to_f", decimal_to_f, 0);
    rb_define_method(cDecimal, "abs", decimal_abs, 0);
    rb_define_method(cDecimal, "zero?", decimal_zero_p, 0);

    rb_define_method(cDecimal, "to_i", decimal_to_i, 0);
    rb_define_method(cDecimal, "to_int", decimal_to_i, 0);
    rb_define_method(cDecimal, "truncate",  decimal_truncate, -1);
    rb_define_method(cDecimal, "floor", decimal_floor, -1);
    rb_define_method(cDecimal, "ceil", decimal_ceil, -1);
    rb_define_method(cDecimal, "round", decimal_round, -1);

    rb_define_method(cDecimal, "nan?", decimal_is_nan_p, 0);
    rb_define_method(cDecimal, "finite?", decimal_is_finite_p, 0);
    rb_define_method(cDecimal, "infinite?", decimal_is_infinite_p, 0);
}
