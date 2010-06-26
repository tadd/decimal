/*
 * Ruby's Integer part from ruby_1_9_1, r27979.
 *
 * These are hand copies (with few modifications) taken from original
 * Ruby's code in "numeric.c" and "bignum.c," so the copyrights are
 * held by matz and other contributors:
 *
 * Copyright (C) 1993-2010 Yukihiro Matsumoto
 *   
 */

/*
 * copied from bignum.c
 */

#define BDIGITS(x) (RBIGNUM_DIGITS(x))

#ifndef HAVE_RB_BIGZEROP
#define rb_bigzero_p bigzero_p

#define BIGZEROP(x) (RBIGNUM_LEN(x) == 0 || \
		     (BDIGITS(x)[0] == 0 && \
		      (RBIGNUM_LEN(x) == 1 || bigzero_p(x))))

static int
bigzero_p(VALUE x)
{
    long i;
    for (i = RBIGNUM_LEN(x) - 1; 0 <= i; i--) {
	if (BDIGITS(x)[i]) return 0;
    }
    return 1;
}
#endif

static VALUE
rb_big_uminus(VALUE x)
{
    VALUE z = rb_big_clone(x);

    RBIGNUM_SET_SIGN(z, !RBIGNUM_SIGN(x));

    return rb_big_norm(z); /* modified to use exported one */
}

static VALUE
rb_big_hash(VALUE x)
{
    int hash;

    hash = rb_memhash(BDIGITS(x), sizeof(BDIGIT)*RBIGNUM_LEN(x)) ^ RBIGNUM_SIGN(x);
    return INT2FIX(hash);
}

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
    double value = RFLOAT_VALUE(flt);
    char *p, *e;

    if (isinf(value))
	return rb_usascii_str_new2(value < 0 ? "-Infinity" : "Infinity");
    else if(isnan(value))
	return rb_usascii_str_new2("NaN");

    snprintf(buf, sizeof(buf), "%#.15g", value); /* ensure to print decimal point */
    if (!(e = strchr(buf, 'e'))) {
	e = buf + strlen(buf);
    }
    if (!ISDIGIT(e[-1])) { /* reformat if ended with decimal point (ex 111111111111111.) */
	snprintf(buf, sizeof(buf), "%#.14e", value);
	if (!(e = strchr(buf, 'e'))) {
	    e = buf + strlen(buf);
	}
    }
    p = e;
    while (p[-1]=='0' && ISDIGIT(p[-2]))
	p--;
    memmove(p, e, strlen(e)+1);
    return rb_usascii_str_new2(buf);
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
    /* modified */
    x = rb_int2big(FIX2LONG(x));    
    return rb_big_minus(x, y);
}

#define SQRT_LONG_MAX ((SIGNED_VALUE)1<<((SIZEOF_LONG*CHAR_BIT-1)/2))
/*tests if N*N would overflow*/
#define FIT_SQRT_LONG(n) (((n)<SQRT_LONG_MAX)&&((n)>=-SQRT_LONG_MAX))

static VALUE
fix_mul(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
#ifdef __HP_cc
/* avoids an optimization bug of HP aC++/ANSI C B3910B A.06.05 [Jul 25 2005] */
	volatile
#endif
	long a, b;
#if SIZEOF_LONG * 2 <= SIZEOF_LONG_LONG
	LONG_LONG d;
#else
	long c;
	VALUE r;
#endif

	a = FIX2LONG(x);
	b = FIX2LONG(y);

#if SIZEOF_LONG * 2 <= SIZEOF_LONG_LONG
	d = (LONG_LONG)a * b;
	if (FIXABLE(d)) return LONG2FIX(d);
	return rb_ll2inum(d);
#else
	if (FIT_SQRT_LONG(a) && FIT_SQRT_LONG(b))
	    return LONG2FIX(a*b);
	c = a * b;
	r = LONG2FIX(c);

	if (a == 0) return x;
	if (FIX2LONG(r) != c || c/a != b) {
	    r = rb_big_mul(rb_int2big(a), rb_int2big(b));
	}
	return r;
#endif
    }
    /* modified */
    return rb_big_mul(y, x);
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

/* extracted from fix_divide() */
static VALUE
fix_div(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long div;

	fixdivmod(FIX2LONG(x), FIX2LONG(y), &div, 0);
	return LONG2NUM(div);
    }
    /* modified */
    x = rb_int2big(FIX2LONG(x));
    return rb_big_div(x, y);
}

static VALUE
fix_divmod(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	long div, mod;

	fixdivmod(FIX2LONG(x), FIX2LONG(y), &div, &mod);

	return rb_assoc_new(LONG2NUM(div), LONG2NUM(mod));
    }
    /* modified */
    x = rb_int2big(FIX2LONG(x));
    return rb_big_divmod(x, y);
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
	    if (!FIT_SQRT_LONG(x)) {
		VALUE v;
	      bignum:
		v = rb_big_pow(rb_int2big(x), LONG2NUM(y));
		if (z != 1) v = rb_big_mul(rb_int2big(neg ? -z : z), v);
		return v;
	    }
	    x = x * x;
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

static VALUE fix_odd_p(VALUE num);

static VALUE
fix_pow(VALUE x, VALUE y)
{
    /* static const double zero = 0.0; */
    long a = FIX2LONG(x);

    if (FIXNUM_P(y)) {
	long b = FIX2LONG(y);

	if (b < 0)
	    return rb_funcall(rb_rational_raw1(x), rb_intern("**"), 1, y);

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
	return int_pow(a, b);
    }
    /* modified */
    if (rb_funcall(y, '<', 1, INT2FIX(0)))
      return rb_funcall(rb_rational_raw1(x), rb_intern("**"), 1, y);
    
    if (a == 0) return INT2FIX(0);
    if (a == 1) return INT2FIX(1);
    if (a == -1) {
      /* modified */
#define int_even_p(x) \
      (FIXNUM_P(x) ? !fix_odd_p(x) : !rb_big_odd_p(x))
      if (int_even_p(y)) return INT2FIX(1);
#undef int_even_p
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
	if (FIX2LONG(x) > FIX2LONG(y)) return INT2FIX(1);
	return INT2FIX(-1);
    }
    /* modified */
    return rb_big_cmp(rb_int2big(FIX2LONG(x)), y);
}

static VALUE
fix_odd_p(VALUE num)
{
    if (num & 2) {
        return Qtrue;
    }
    return Qfalse;
}
