/*
 * Ruby's Integer part from ruby_1_8, r23740.
 *
 * These are hand copies (with few modifications) taken from original
 * Ruby's code in "numeric.c" and "bignum.c," so the copyrights are
 * held by matz and other contributors:
 *
 * Copyright (C) 1993-2009 Yukihiro Matsumoto
 *   
 */

/*
 * copied from bignum.c
 */

#define BDIGITS(x) ((BDIGIT*)RBIGNUM(x)->digits)
#define BIGZEROP(x) (RBIGNUM(x)->len == 0 || \
		     (BDIGITS(x)[0] == 0 && \
		      (RBIGNUM(x)->len == 1 || bigzero_p(x))))

static int
bigzero_p(VALUE x)
{
    long i;
    for (i = 0; i < RBIGNUM(x)->len; ++i) {
       if (BDIGITS(x)[i]) return 0;
    }
    return 1;
}

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

static VALUE
rb_big_uminus(VALUE x)
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

/* specially, copied from ruby_1_9_1 */
static VALUE
rb_big_odd_p(VALUE num)
{
    if (BDIGITS(num)[0] & 1) {
	return Qtrue;
    }
    return Qfalse;
}

/*
 * copied from numeric.c
 */

static VALUE
flo_to_s(VALUE flt)
{
    char buf[32];
    double value = RFLOAT(flt)->value;
    char *p, *e;

    if (isinf(value))
	return rb_str_new2(value < 0 ? "-Infinity" : "Infinity");
    else if(isnan(value))
	return rb_str_new2("NaN");

    sprintf(buf, "%#.15g", value); /* ensure to print decimal point */
    if (!(e = strchr(buf, 'e'))) {
	e = buf + strlen(buf);
    }
    if (!ISDIGIT(e[-1])) { /* reformat if ended with decimal point (ex 111111111111111.) */
	sprintf(buf, "%#.14e", value);
	if (!(e = strchr(buf, 'e'))) {
	    e = buf + strlen(buf);
	}
    }
    p = e;
    while (p[-1]=='0' && ISDIGIT(p[-2]))
	p--;
    memmove(p, e, strlen(e)+1);
    return rb_str_new2(buf);
}

static VALUE
fix_plus(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long a, b, c;
	VALUE r;

	a = FIX2LONG(x);
	b = FIX2LONG(y);
	c = a + b;
	r = LONG2NUM(c);

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
	r = LONG2NUM(c);

	return r;
    }
    return rb_big_minus(rb_int2big(FIX2LONG(x)), y); /* modified */
}

static VALUE
fix_mul(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
#ifdef __HP_cc
        /* avoids an optimization bug of HP aC++/ANSI C B3910B A.06.05 [Jul 25 2005] */
        volatile
#endif
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
    return rb_big_div(rb_int2big(FIX2LONG(x)), y);
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
int_pow(long x, unsigned long y)
{
    int neg = x < 0;
    long z = 1;

    if (neg) x = -x;
    if (y & 1)
	z = x;
    else
	neg = 0;
    y &= ~1;
    do {
	while (y % 2 == 0) {
	    long x2 = x * x;
	    if (x2/x != x || !POSFIXABLE(x2)) {
		VALUE v;
	      bignum:
		v = rb_big_pow(rb_int2big(x), LONG2NUM(y));
		if (z != 1) v = rb_big_mul(rb_int2big(neg ? -z : z), v);
		return v;
	    }
	    x = x2;
	    y >>= 1;
	}
	{
	    long xz = x * z;
	    if (!POSFIXABLE(xz) || xz / x != z) {
		goto bignum;
 	    }
	    z = xz;
	}
    } while (--y);
    if (neg) z = -z;
    return LONG2NUM(z);
}

static VALUE
fix_pow(VALUE x, VALUE y)
{
    long a = FIX2LONG(x);

    if (FIXNUM_P(y)) {
	long b = FIX2LONG(y);

	if (b == 0) return INT2FIX(1);
	if (b == 1) return x;
	if (a == 0) {
	    if (b > 0) return INT2FIX(0);
	    /* modified */
            rb_bug("fix_pow(): infinity returned");
	    return Qnil;
	}
	if (a == 1) return INT2FIX(1);
	if (a == -1) {
	    if (b % 2 == 0)
		return INT2FIX(1);
	    else 
		return INT2FIX(-1);
	}
	if (b > 0) {
	    return int_pow(a, b);
	}
        /* modified */
        rb_bug("fix_pow(): Float returned");
	return Qnil;
    }
    /* modified to treat with Bignums only */
    if (a == 0) return INT2FIX(0);
    if (a == 1) return INT2FIX(1);
    if (a == -1) {
        if (!rb_big_odd_p(y)) return INT2FIX(1); /* modified */
        else return INT2FIX(-1);
    }
    x = rb_int2big(FIX2LONG(x));
    return rb_big_pow(x, y);
}

static VALUE
fix_equal(VALUE x, VALUE y)
{
    if (x == y) return Qtrue;
    if (FIXNUM_P(y)) return Qfalse;
    return rb_big_eq(y, x); /* modified */
}

static VALUE
fix_cmp(VALUE x, VALUE y)
{
    if (x == y) return INT2FIX(0);
    if (FIXNUM_P(y)) {
	long a = FIX2LONG(x), b = FIX2LONG(y);

	if (a > b) return INT2FIX(1);
	return INT2FIX(-1);
    }
    else {
	return rb_big_cmp(rb_int2big(FIX2LONG(x)), y); /* modified */
    }
}

static VALUE
fix_odd_p(VALUE num)
{
    if (num & 2) {
        return Qtrue;
    }
    return Qfalse;
}
