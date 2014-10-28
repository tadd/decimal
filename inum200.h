/*
 * Ruby's Integer part from ruby_2_0_0, r47627.
 *
 * These are hand copies (with few modifications) taken from original
 * Ruby's code in "numeric.c" and "bignum.c," so the copyrights are
 * held by matz and other contributors:
 *
 * Copyright (C) 1993-2014 Yukihiro Matsumoto
 *
 */

/*
 * copied from internal.h
 */

#define MUL_OVERFLOW_SIGNED_INTEGER_P(a, b, min, max) ( \
    (a) == 0 ? 0 : \
    (a) == -1 ? (b) < -(max) : \
    (a) > 0 ? \
      ((b) > 0 ? (max) / (a) < (b) : (min) / (a) > (b)) : \
      ((b) > 0 ? (min) / (a) < (b) : (max) / (a) > (b)))
#define MUL_OVERFLOW_FIXNUM_P(a, b) MUL_OVERFLOW_SIGNED_INTEGER_P(a, b, FIXNUM_MIN, FIXNUM_MAX)

/*
 * copied from util.c
 */

#include <errno.h>

#ifdef WORDS_BIGENDIAN
#define IEEE_BIG_ENDIAN
#else
#define IEEE_LITTLE_ENDIAN
#endif

#ifdef __vax__
#define VAX
#undef IEEE_BIG_ENDIAN
#undef IEEE_LITTLE_ENDIAN
#endif

#if defined(__arm__) && !defined(__VFP_FP__)
#define IEEE_BIG_ENDIAN
#undef IEEE_LITTLE_ENDIAN
#endif

#undef Long
#undef ULong

#if SIZEOF_INT == 4
#define Long int
#define ULong unsigned int
#elif SIZEOF_LONG == 4
#define Long long int
#define ULong unsigned long int
#endif

#if HAVE_LONG_LONG
#define Llong LONG_LONG
#endif

#ifdef DEBUG
#include "stdio.h"
#define Bug(x) {fprintf(stderr, "%s\n", (x)); exit(EXIT_FAILURE);}
#endif

#include "stdlib.h"
#include "string.h"

#ifdef USE_LOCALE
#include "locale.h"
#endif

#ifdef MALLOC
extern void *MALLOC(size_t);
#else
#define MALLOC malloc
#endif
#ifdef FREE
extern void FREE(void*);
#else
#define FREE free
#endif

#ifndef Omit_Private_Memory
#ifndef PRIVATE_MEM
#define PRIVATE_MEM 2304
#endif
#define PRIVATE_mem ((PRIVATE_MEM+sizeof(double)-1)/sizeof(double))
static double private_mem[PRIVATE_mem], *pmem_next = private_mem;
#endif

#undef IEEE_Arith
#undef Avoid_Underflow
#ifdef IEEE_BIG_ENDIAN
#define IEEE_Arith
#endif
#ifdef IEEE_LITTLE_ENDIAN
#define IEEE_Arith
#endif

#ifdef Bad_float_h

#ifdef IEEE_Arith
#define DBL_DIG 15
#define DBL_MAX_10_EXP 308
#define DBL_MAX_EXP 1024
#define FLT_RADIX 2
#endif /*IEEE_Arith*/

#ifdef IBM
#define DBL_DIG 16
#define DBL_MAX_10_EXP 75
#define DBL_MAX_EXP 63
#define FLT_RADIX 16
#define DBL_MAX 7.2370055773322621e+75
#endif

#ifdef VAX
#define DBL_DIG 16
#define DBL_MAX_10_EXP 38
#define DBL_MAX_EXP 127
#define FLT_RADIX 2
#define DBL_MAX 1.7014118346046923e+38
#endif

#ifndef LONG_MAX
#define LONG_MAX 2147483647
#endif

#else /* ifndef Bad_float_h */
#include "float.h"
#endif /* Bad_float_h */

#ifndef __MATH_H__
#include "math.h"
#endif

#ifdef __cplusplus
extern "C" {
#if 0
} /* satisfy cc-mode */
#endif
#endif

#if defined(IEEE_LITTLE_ENDIAN) + defined(IEEE_BIG_ENDIAN) + defined(VAX) + defined(IBM) != 1
Exactly one of IEEE_LITTLE_ENDIAN, IEEE_BIG_ENDIAN, VAX, or IBM should be defined.
#endif

typedef union { double d; ULong L[2]; } U;

#ifdef YES_ALIAS
typedef double double_u;
#  define dval(x) (x)
#  ifdef IEEE_LITTLE_ENDIAN
#    define word0(x) (((ULong *)&(x))[1])
#    define word1(x) (((ULong *)&(x))[0])
#  else
#    define word0(x) (((ULong *)&(x))[0])
#    define word1(x) (((ULong *)&(x))[1])
#  endif
#else
typedef U double_u;
#  ifdef IEEE_LITTLE_ENDIAN
#    define word0(x) ((x).L[1])
#    define word1(x) ((x).L[0])
#  else
#    define word0(x) ((x).L[0])
#    define word1(x) ((x).L[1])
#  endif
#  define dval(x) ((x).d)
#endif

/* The following definition of Storeinc is appropriate for MIPS processors.
 * An alternative that might be better on some machines is
 * #define Storeinc(a,b,c) (*a++ = b << 16 | c & 0xffff)
 */
#if defined(IEEE_LITTLE_ENDIAN) + defined(VAX) + defined(__arm__)
#define Storeinc(a,b,c) (((unsigned short *)(a))[1] = (unsigned short)(b), \
((unsigned short *)(a))[0] = (unsigned short)(c), (a)++)
#else
#define Storeinc(a,b,c) (((unsigned short *)(a))[0] = (unsigned short)(b), \
((unsigned short *)(a))[1] = (unsigned short)(c), (a)++)
#endif

/* #define P DBL_MANT_DIG */
/* Ten_pmax = floor(P*log(2)/log(5)) */
/* Bletch = (highest power of 2 < DBL_MAX_10_EXP) / 16 */
/* Quick_max = floor((P-1)*log(FLT_RADIX)/log(10) - 1) */
/* Int_max = floor(P*log(FLT_RADIX)/log(10) - 1) */

#ifdef IEEE_Arith
#define Exp_shift  20
#define Exp_shift1 20
#define Exp_msk1    0x100000
#define Exp_msk11   0x100000
#define Exp_mask  0x7ff00000
#define P 53
#define Bias 1023
#define Emin (-1022)
#define Exp_1  0x3ff00000
#define Exp_11 0x3ff00000
#define Ebits 11
#define Frac_mask  0xfffff
#define Frac_mask1 0xfffff
#define Ten_pmax 22
#define Bletch 0x10
#define Bndry_mask  0xfffff
#define Bndry_mask1 0xfffff
#define LSB 1
#define Sign_bit 0x80000000
#define Log2P 1
#define Tiny0 0
#define Tiny1 1
#define Quick_max 14
#define Int_max 14
#ifndef NO_IEEE_Scale
#define Avoid_Underflow
#ifdef Flush_Denorm	/* debugging option */
#undef Sudden_Underflow
#endif
#endif

#ifndef Flt_Rounds
#ifdef FLT_ROUNDS
#define Flt_Rounds FLT_ROUNDS
#else
#define Flt_Rounds 1
#endif
#endif /*Flt_Rounds*/

#ifdef Honor_FLT_ROUNDS
#define Rounding rounding
#undef Check_FLT_ROUNDS
#define Check_FLT_ROUNDS
#else
#define Rounding Flt_Rounds
#endif

#else /* ifndef IEEE_Arith */
#undef Check_FLT_ROUNDS
#undef Honor_FLT_ROUNDS
#undef SET_INEXACT
#undef  Sudden_Underflow
#define Sudden_Underflow
#ifdef IBM
#undef Flt_Rounds
#define Flt_Rounds 0
#define Exp_shift  24
#define Exp_shift1 24
#define Exp_msk1   0x1000000
#define Exp_msk11  0x1000000
#define Exp_mask  0x7f000000
#define P 14
#define Bias 65
#define Exp_1  0x41000000
#define Exp_11 0x41000000
#define Ebits 8	/* exponent has 7 bits, but 8 is the right value in b2d */
#define Frac_mask  0xffffff
#define Frac_mask1 0xffffff
#define Bletch 4
#define Ten_pmax 22
#define Bndry_mask  0xefffff
#define Bndry_mask1 0xffffff
#define LSB 1
#define Sign_bit 0x80000000
#define Log2P 4
#define Tiny0 0x100000
#define Tiny1 0
#define Quick_max 14
#define Int_max 15
#else /* VAX */
#undef Flt_Rounds
#define Flt_Rounds 1
#define Exp_shift  23
#define Exp_shift1 7
#define Exp_msk1    0x80
#define Exp_msk11   0x800000
#define Exp_mask  0x7f80
#define P 56
#define Bias 129
#define Exp_1  0x40800000
#define Exp_11 0x4080
#define Ebits 8
#define Frac_mask  0x7fffff
#define Frac_mask1 0xffff007f
#define Ten_pmax 24
#define Bletch 2
#define Bndry_mask  0xffff007f
#define Bndry_mask1 0xffff007f
#define LSB 0x10000
#define Sign_bit 0x8000
#define Log2P 1
#define Tiny0 0x80
#define Tiny1 0
#define Quick_max 15
#define Int_max 15
#endif /* IBM, VAX */
#endif /* IEEE_Arith */

#ifndef IEEE_Arith
#define ROUND_BIASED
#endif

#ifdef RND_PRODQUOT
#define rounded_product(a,b) ((a) = rnd_prod((a), (b)))
#define rounded_quotient(a,b) ((a) = rnd_quot((a), (b)))
extern double rnd_prod(double, double), rnd_quot(double, double);
#else
#define rounded_product(a,b) ((a) *= (b))
#define rounded_quotient(a,b) ((a) /= (b))
#endif

#define Big0 (Frac_mask1 | Exp_msk1*(DBL_MAX_EXP+Bias-1))
#define Big1 0xffffffff

#ifndef Pack_32
#define Pack_32
#endif

#define FFFFFFFF 0xffffffffUL

#ifdef NO_LONG_LONG
#undef ULLong
#ifdef Just_16
#undef Pack_32
/* When Pack_32 is not defined, we store 16 bits per 32-bit Long.
 * This makes some inner loops simpler and sometimes saves work
 * during multiplications, but it often seems to make things slightly
 * slower.  Hence the default is now to store 32 bits per Long.
 */
#endif
#else	/* long long available */
#ifndef Llong
#define Llong long long
#endif
#ifndef ULLong
#define ULLong unsigned Llong
#endif
#endif /* NO_LONG_LONG */

#define MULTIPLE_THREADS 1

#ifndef MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n)	/*nothing*/
#define FREE_DTOA_LOCK(n)	/*nothing*/
#else
#define ACQUIRE_DTOA_LOCK(n)	/*unused right now*/
#define FREE_DTOA_LOCK(n)	/*unused right now*/
#endif

#define Kmax 15

struct Bigint {
    struct Bigint *next;
    int k, maxwds, sign, wds;
    ULong x[1];
};

typedef struct Bigint Bigint;

static Bigint *freelist[Kmax+1];

static Bigint *
Balloc(int k)
{
    int x;
    Bigint *rv;
#ifndef Omit_Private_Memory
    size_t len;
#endif

    ACQUIRE_DTOA_LOCK(0);
    if (k <= Kmax && (rv = freelist[k]) != 0) {
        freelist[k] = rv->next;
    }
    else {
        x = 1 << k;
#ifdef Omit_Private_Memory
        rv = (Bigint *)MALLOC(sizeof(Bigint) + (x-1)*sizeof(ULong));
#else
        len = (sizeof(Bigint) + (x-1)*sizeof(ULong) + sizeof(double) - 1)
                /sizeof(double);
        if (k <= Kmax && pmem_next - private_mem + len <= PRIVATE_mem) {
            rv = (Bigint*)pmem_next;
            pmem_next += len;
        }
        else
            rv = (Bigint*)MALLOC(len*sizeof(double));
#endif
        rv->k = k;
        rv->maxwds = x;
    }
    FREE_DTOA_LOCK(0);
    rv->sign = rv->wds = 0;
    return rv;
}

static void
Bfree(Bigint *v)
{
    if (v) {
        if (v->k > Kmax) {
            FREE(v);
            return;
        }
        ACQUIRE_DTOA_LOCK(0);
        v->next = freelist[v->k];
        freelist[v->k] = v;
        FREE_DTOA_LOCK(0);
    }
}

#define Bcopy(x,y) memcpy((char *)&(x)->sign, (char *)&(y)->sign, \
(y)->wds*sizeof(Long) + 2*sizeof(int))

static Bigint *
multadd(Bigint *b, int m, int a)   /* multiply by m and add a */
{
    int i, wds;
    ULong *x;
#ifdef ULLong
    ULLong carry, y;
#else
    ULong carry, y;
#ifdef Pack_32
    ULong xi, z;
#endif
#endif
    Bigint *b1;

    wds = b->wds;
    x = b->x;
    i = 0;
    carry = a;
    do {
#ifdef ULLong
        y = *x * (ULLong)m + carry;
        carry = y >> 32;
        *x++ = (ULong)(y & FFFFFFFF);
#else
#ifdef Pack_32
        xi = *x;
        y = (xi & 0xffff) * m + carry;
        z = (xi >> 16) * m + (y >> 16);
        carry = z >> 16;
        *x++ = (z << 16) + (y & 0xffff);
#else
        y = *x * m + carry;
        carry = y >> 16;
        *x++ = y & 0xffff;
#endif
#endif
    } while (++i < wds);
    if (carry) {
        if (wds >= b->maxwds) {
            b1 = Balloc(b->k+1);
            Bcopy(b1, b);
            Bfree(b);
            b = b1;
        }
        b->x[wds++] = (ULong)carry;
        b->wds = wds;
    }
    return b;
}

/* removed: s2b() */

static int
hi0bits(register ULong x)
{
    register int k = 0;

    if (!(x & 0xffff0000)) {
        k = 16;
        x <<= 16;
    }
    if (!(x & 0xff000000)) {
        k += 8;
        x <<= 8;
    }
    if (!(x & 0xf0000000)) {
        k += 4;
        x <<= 4;
    }
    if (!(x & 0xc0000000)) {
        k += 2;
        x <<= 2;
    }
    if (!(x & 0x80000000)) {
        k++;
        if (!(x & 0x40000000))
            return 32;
    }
    return k;
}

static int
lo0bits(ULong *y)
{
    register int k;
    register ULong x = *y;

    if (x & 7) {
        if (x & 1)
            return 0;
        if (x & 2) {
            *y = x >> 1;
            return 1;
        }
        *y = x >> 2;
        return 2;
    }
    k = 0;
    if (!(x & 0xffff)) {
        k = 16;
        x >>= 16;
    }
    if (!(x & 0xff)) {
        k += 8;
        x >>= 8;
    }
    if (!(x & 0xf)) {
        k += 4;
        x >>= 4;
    }
    if (!(x & 0x3)) {
        k += 2;
        x >>= 2;
    }
    if (!(x & 1)) {
        k++;
        x >>= 1;
        if (!x)
            return 32;
    }
    *y = x;
    return k;
}

static Bigint *
i2b(int i)
{
    Bigint *b;

    b = Balloc(1);
    b->x[0] = i;
    b->wds = 1;
    return b;
}

static Bigint *
mult(Bigint *a, Bigint *b)
{
    Bigint *c;
    int k, wa, wb, wc;
    ULong *x, *xa, *xae, *xb, *xbe, *xc, *xc0;
    ULong y;
#ifdef ULLong
    ULLong carry, z;
#else
    ULong carry, z;
#ifdef Pack_32
    ULong z2;
#endif
#endif

    if (a->wds < b->wds) {
        c = a;
        a = b;
        b = c;
    }
    k = a->k;
    wa = a->wds;
    wb = b->wds;
    wc = wa + wb;
    if (wc > a->maxwds)
        k++;
    c = Balloc(k);
    for (x = c->x, xa = x + wc; x < xa; x++)
        *x = 0;
    xa = a->x;
    xae = xa + wa;
    xb = b->x;
    xbe = xb + wb;
    xc0 = c->x;
#ifdef ULLong
    for (; xb < xbe; xc0++) {
        if ((y = *xb++) != 0) {
            x = xa;
            xc = xc0;
            carry = 0;
            do {
                z = *x++ * (ULLong)y + *xc + carry;
                carry = z >> 32;
                *xc++ = (ULong)(z & FFFFFFFF);
            } while (x < xae);
            *xc = (ULong)carry;
        }
    }
#else
#ifdef Pack_32
    for (; xb < xbe; xb++, xc0++) {
        if (y = *xb & 0xffff) {
            x = xa;
            xc = xc0;
            carry = 0;
            do {
                z = (*x & 0xffff) * y + (*xc & 0xffff) + carry;
                carry = z >> 16;
                z2 = (*x++ >> 16) * y + (*xc >> 16) + carry;
                carry = z2 >> 16;
                Storeinc(xc, z2, z);
            } while (x < xae);
            *xc = (ULong)carry;
        }
        if (y = *xb >> 16) {
            x = xa;
            xc = xc0;
            carry = 0;
            z2 = *xc;
            do {
                z = (*x & 0xffff) * y + (*xc >> 16) + carry;
                carry = z >> 16;
                Storeinc(xc, z, z2);
                z2 = (*x++ >> 16) * y + (*xc & 0xffff) + carry;
                carry = z2 >> 16;
            } while (x < xae);
            *xc = z2;
        }
    }
#else
    for (; xb < xbe; xc0++) {
        if (y = *xb++) {
            x = xa;
            xc = xc0;
            carry = 0;
            do {
                z = *x++ * y + *xc + carry;
                carry = z >> 16;
                *xc++ = z & 0xffff;
            } while (x < xae);
            *xc = (ULong)carry;
        }
    }
#endif
#endif
    for (xc0 = c->x, xc = xc0 + wc; wc > 0 && !*--xc; --wc) ;
    c->wds = wc;
    return c;
}

static Bigint *p5s;

static Bigint *
pow5mult(Bigint *b, int k)
{
    Bigint *b1, *p5, *p51;
    int i;
    static int p05[3] = { 5, 25, 125 };

    if ((i = k & 3) != 0)
        b = multadd(b, p05[i-1], 0);

    if (!(k >>= 2))
        return b;
    if (!(p5 = p5s)) {
        /* first time */
#ifdef MULTIPLE_THREADS
        ACQUIRE_DTOA_LOCK(1);
        if (!(p5 = p5s)) {
            p5 = p5s = i2b(625);
            p5->next = 0;
        }
        FREE_DTOA_LOCK(1);
#else
        p5 = p5s = i2b(625);
        p5->next = 0;
#endif
    }
    for (;;) {
        if (k & 1) {
            b1 = mult(b, p5);
            Bfree(b);
            b = b1;
        }
        if (!(k >>= 1))
            break;
        if (!(p51 = p5->next)) {
#ifdef MULTIPLE_THREADS
            ACQUIRE_DTOA_LOCK(1);
            if (!(p51 = p5->next)) {
                p51 = p5->next = mult(p5,p5);
                p51->next = 0;
            }
            FREE_DTOA_LOCK(1);
#else
            p51 = p5->next = mult(p5,p5);
            p51->next = 0;
#endif
        }
        p5 = p51;
    }
    return b;
}

static Bigint *
lshift(Bigint *b, int k)
{
    int i, k1, n, n1;
    Bigint *b1;
    ULong *x, *x1, *xe, z;

#ifdef Pack_32
    n = k >> 5;
#else
    n = k >> 4;
#endif
    k1 = b->k;
    n1 = n + b->wds + 1;
    for (i = b->maxwds; n1 > i; i <<= 1)
        k1++;
    b1 = Balloc(k1);
    x1 = b1->x;
    for (i = 0; i < n; i++)
        *x1++ = 0;
    x = b->x;
    xe = x + b->wds;
#ifdef Pack_32
    if (k &= 0x1f) {
        k1 = 32 - k;
        z = 0;
        do {
            *x1++ = *x << k | z;
            z = *x++ >> k1;
        } while (x < xe);
        if ((*x1 = z) != 0)
            ++n1;
    }
#else
    if (k &= 0xf) {
        k1 = 16 - k;
        z = 0;
        do {
            *x1++ = *x << k  & 0xffff | z;
            z = *x++ >> k1;
        } while (x < xe);
        if (*x1 = z)
            ++n1;
    }
#endif
    else
        do {
            *x1++ = *x++;
        } while (x < xe);
    b1->wds = n1 - 1;
    Bfree(b);
    return b1;
}

/* renamed from cmp() */
static int
bicmp(Bigint *a, Bigint *b)
{
    ULong *xa, *xa0, *xb, *xb0;
    int i, j;

    i = a->wds;
    j = b->wds;
#ifdef DEBUG
    if (i > 1 && !a->x[i-1])
        Bug("bicmp called with a->x[a->wds-1] == 0");
    if (j > 1 && !b->x[j-1])
        Bug("bicmp called with b->x[b->wds-1] == 0");
#endif
    if (i -= j)
        return i;
    xa0 = a->x;
    xa = xa0 + j;
    xb0 = b->x;
    xb = xb0 + j;
    for (;;) {
        if (*--xa != *--xb)
            return *xa < *xb ? -1 : 1;
        if (xa <= xa0)
            break;
    }
    return 0;
}

static Bigint *
diff(Bigint *a, Bigint *b)
{
    Bigint *c;
    int i, wa, wb;
    ULong *xa, *xae, *xb, *xbe, *xc;
#ifdef ULLong
    ULLong borrow, y;
#else
    ULong borrow, y;
#ifdef Pack_32
    ULong z;
#endif
#endif

    i = bicmp(a,b);
    if (!i) {
        c = Balloc(0);
        c->wds = 1;
        c->x[0] = 0;
        return c;
    }
    if (i < 0) {
        c = a;
        a = b;
        b = c;
        i = 1;
    }
    else
        i = 0;
    c = Balloc(a->k);
    c->sign = i;
    wa = a->wds;
    xa = a->x;
    xae = xa + wa;
    wb = b->wds;
    xb = b->x;
    xbe = xb + wb;
    xc = c->x;
    borrow = 0;
#ifdef ULLong
    do {
        y = (ULLong)*xa++ - *xb++ - borrow;
        borrow = y >> 32 & (ULong)1;
        *xc++ = (ULong)(y & FFFFFFFF);
    } while (xb < xbe);
    while (xa < xae) {
        y = *xa++ - borrow;
        borrow = y >> 32 & (ULong)1;
        *xc++ = (ULong)(y & FFFFFFFF);
    }
#else
#ifdef Pack_32
    do {
        y = (*xa & 0xffff) - (*xb & 0xffff) - borrow;
        borrow = (y & 0x10000) >> 16;
        z = (*xa++ >> 16) - (*xb++ >> 16) - borrow;
        borrow = (z & 0x10000) >> 16;
        Storeinc(xc, z, y);
    } while (xb < xbe);
    while (xa < xae) {
        y = (*xa & 0xffff) - borrow;
        borrow = (y & 0x10000) >> 16;
        z = (*xa++ >> 16) - borrow;
        borrow = (z & 0x10000) >> 16;
        Storeinc(xc, z, y);
    }
#else
    do {
        y = *xa++ - *xb++ - borrow;
        borrow = (y & 0x10000) >> 16;
        *xc++ = y & 0xffff;
    } while (xb < xbe);
    while (xa < xae) {
        y = *xa++ - borrow;
        borrow = (y & 0x10000) >> 16;
        *xc++ = y & 0xffff;
    }
#endif
#endif
    while (!*--xc)
        wa--;
    c->wds = wa;
    return c;
}

/* removed: ulp(), b2d() */

static Bigint *
d2b(double d_, int *e, int *bits)
{
    double_u d;
    Bigint *b;
    int de, k;
    ULong *x, y, z;
#ifndef Sudden_Underflow
    int i;
#endif
#ifdef VAX
    ULong d0, d1;
#endif
    dval(d) = d_;
#ifdef VAX
    d0 = word0(d) >> 16 | word0(d) << 16;
    d1 = word1(d) >> 16 | word1(d) << 16;
#else
#define d0 word0(d)
#define d1 word1(d)
#endif

#ifdef Pack_32
    b = Balloc(1);
#else
    b = Balloc(2);
#endif
    x = b->x;

    z = d0 & Frac_mask;
    d0 &= 0x7fffffff;   /* clear sign bit, which we ignore */
#ifdef Sudden_Underflow
    de = (int)(d0 >> Exp_shift);
#ifndef IBM
    z |= Exp_msk11;
#endif
#else
    if ((de = (int)(d0 >> Exp_shift)) != 0)
        z |= Exp_msk1;
#endif
#ifdef Pack_32
    if ((y = d1) != 0) {
        if ((k = lo0bits(&y)) != 0) {
            x[0] = y | z << (32 - k);
            z >>= k;
        }
        else
            x[0] = y;
#ifndef Sudden_Underflow
        i =
#endif
        b->wds = (x[1] = z) ? 2 : 1;
    }
    else {
#ifdef DEBUG
        if (!z)
            Bug("Zero passed to d2b");
#endif
        k = lo0bits(&z);
        x[0] = z;
#ifndef Sudden_Underflow
        i =
#endif
        b->wds = 1;
        k += 32;
    }
#else
    if (y = d1) {
        if (k = lo0bits(&y))
            if (k >= 16) {
                x[0] = y | z << 32 - k & 0xffff;
                x[1] = z >> k - 16 & 0xffff;
                x[2] = z >> k;
                i = 2;
            }
            else {
                x[0] = y & 0xffff;
                x[1] = y >> 16 | z << 16 - k & 0xffff;
                x[2] = z >> k & 0xffff;
                x[3] = z >> k+16;
                i = 3;
            }
        else {
            x[0] = y & 0xffff;
            x[1] = y >> 16;
            x[2] = z & 0xffff;
            x[3] = z >> 16;
            i = 3;
        }
    }
    else {
#ifdef DEBUG
        if (!z)
            Bug("Zero passed to d2b");
#endif
        k = lo0bits(&z);
        if (k >= 16) {
            x[0] = z;
            i = 0;
        }
        else {
            x[0] = z & 0xffff;
            x[1] = z >> 16;
            i = 1;
        }
        k += 32;
    }
    while (!x[i])
        --i;
    b->wds = i + 1;
#endif
#ifndef Sudden_Underflow
    if (de) {
#endif
#ifdef IBM
        *e = (de - Bias - (P-1) << 2) + k;
        *bits = 4*P + 8 - k - hi0bits(word0(d) & Frac_mask);
#else
        *e = de - Bias - (P-1) + k;
        *bits = P - k;
#endif
#ifndef Sudden_Underflow
    }
    else {
        *e = de - Bias - (P-1) + 1 + k;
#ifdef Pack_32
        *bits = 32*i - hi0bits(x[i-1]);
#else
        *bits = (i+2)*16 - hi0bits(x[i]);
#endif
    }
#endif
    return b;
}
#undef d0
#undef d1

/* removed: ratio() */

static const double
tens[] = {
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
    1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
    1e20, 1e21, 1e22
#ifdef VAX
    , 1e23, 1e24
#endif
};

static const double
#ifdef IEEE_Arith
bigtens[] = { 1e16, 1e32, 1e64, 1e128, 1e256 };
static const double tinytens[] = { 1e-16, 1e-32, 1e-64, 1e-128,
#ifdef Avoid_Underflow
    9007199254740992.*9007199254740992.e-256
    /* = 2^106 * 1e-53 */
#else
    1e-256
#endif
};
/* The factor of 2^53 in tinytens[4] helps us avoid setting the underflow */
/* flag unnecessarily.  It leads to a song and dance at the end of strtod. */
#define Scale_Bit 0x10
#define n_bigtens 5
#else
#ifdef IBM
bigtens[] = { 1e16, 1e32, 1e64 };
static const double tinytens[] = { 1e-16, 1e-32, 1e-64 };
#define n_bigtens 3
#else
bigtens[] = { 1e16, 1e32 };
static const double tinytens[] = { 1e-16, 1e-32 };
#define n_bigtens 2
#endif
#endif

#ifndef IEEE_Arith
#undef INFNAN_CHECK
#endif

#ifdef INFNAN_CHECK

#ifndef NAN_WORD0
#define NAN_WORD0 0x7ff80000
#endif

#ifndef NAN_WORD1
#define NAN_WORD1 0
#endif

static int
match(const char **sp, char *t)
{
    int c, d;
    const char *s = *sp;

    while (d = *t++) {
        if ((c = *++s) >= 'A' && c <= 'Z')
            c += 'a' - 'A';
        if (c != d)
            return 0;
    }
    *sp = s + 1;
    return 1;
}

#ifndef No_Hex_NaN
static void
hexnan(double *rvp, const char **sp)
{
    ULong c, x[2];
    const char *s;
    int havedig, udx0, xshift;

    x[0] = x[1] = 0;
    havedig = xshift = 0;
    udx0 = 1;
    s = *sp;
    while (c = *(const unsigned char*)++s) {
        if (c >= '0' && c <= '9')
            c -= '0';
        else if (c >= 'a' && c <= 'f')
            c += 10 - 'a';
        else if (c >= 'A' && c <= 'F')
            c += 10 - 'A';
        else if (c <= ' ') {
            if (udx0 && havedig) {
                udx0 = 0;
                xshift = 1;
            }
            continue;
        }
        else if (/*(*/ c == ')' && havedig) {
            *sp = s + 1;
            break;
        }
        else
            return; /* invalid form: don't change *sp */
        havedig = 1;
        if (xshift) {
            xshift = 0;
            x[0] = x[1];
            x[1] = 0;
        }
        if (udx0)
            x[0] = (x[0] << 4) | (x[1] >> 28);
        x[1] = (x[1] << 4) | c;
    }
    if ((x[0] &= 0xfffff) || x[1]) {
        word0(*rvp) = Exp_mask | x[0];
        word1(*rvp) = x[1];
    }
}
#endif /*No_Hex_NaN*/
#endif /* INFNAN_CHECK */

/* removed: ruby_strtod() */

static int
quorem(Bigint *b, Bigint *S)
{
    int n;
    ULong *bx, *bxe, q, *sx, *sxe;
#ifdef ULLong
    ULLong borrow, carry, y, ys;
#else
    ULong borrow, carry, y, ys;
#ifdef Pack_32
    ULong si, z, zs;
#endif
#endif

    n = S->wds;
#ifdef DEBUG
    /*debug*/ if (b->wds > n)
    /*debug*/   Bug("oversize b in quorem");
#endif
    if (b->wds < n)
        return 0;
    sx = S->x;
    sxe = sx + --n;
    bx = b->x;
    bxe = bx + n;
    q = *bxe / (*sxe + 1);  /* ensure q <= true quotient */
#ifdef DEBUG
    /*debug*/ if (q > 9)
    /*debug*/   Bug("oversized quotient in quorem");
#endif
    if (q) {
        borrow = 0;
        carry = 0;
        do {
#ifdef ULLong
            ys = *sx++ * (ULLong)q + carry;
            carry = ys >> 32;
            y = *bx - (ys & FFFFFFFF) - borrow;
            borrow = y >> 32 & (ULong)1;
            *bx++ = (ULong)(y & FFFFFFFF);
#else
#ifdef Pack_32
            si = *sx++;
            ys = (si & 0xffff) * q + carry;
            zs = (si >> 16) * q + (ys >> 16);
            carry = zs >> 16;
            y = (*bx & 0xffff) - (ys & 0xffff) - borrow;
            borrow = (y & 0x10000) >> 16;
            z = (*bx >> 16) - (zs & 0xffff) - borrow;
            borrow = (z & 0x10000) >> 16;
            Storeinc(bx, z, y);
#else
            ys = *sx++ * q + carry;
            carry = ys >> 16;
            y = *bx - (ys & 0xffff) - borrow;
            borrow = (y & 0x10000) >> 16;
            *bx++ = y & 0xffff;
#endif
#endif
        } while (sx <= sxe);
        if (!*bxe) {
            bx = b->x;
            while (--bxe > bx && !*bxe)
                --n;
            b->wds = n;
        }
    }
    if (bicmp(b, S) >= 0) {
        q++;
        borrow = 0;
        carry = 0;
        bx = b->x;
        sx = S->x;
        do {
#ifdef ULLong
            ys = *sx++ + carry;
            carry = ys >> 32;
            y = *bx - (ys & FFFFFFFF) - borrow;
            borrow = y >> 32 & (ULong)1;
            *bx++ = (ULong)(y & FFFFFFFF);
#else
#ifdef Pack_32
            si = *sx++;
            ys = (si & 0xffff) + carry;
            zs = (si >> 16) + (ys >> 16);
            carry = zs >> 16;
            y = (*bx & 0xffff) - (ys & 0xffff) - borrow;
            borrow = (y & 0x10000) >> 16;
            z = (*bx >> 16) - (zs & 0xffff) - borrow;
            borrow = (z & 0x10000) >> 16;
            Storeinc(bx, z, y);
#else
            ys = *sx++ + carry;
            carry = ys >> 16;
            y = *bx - (ys & 0xffff) - borrow;
            borrow = (y & 0x10000) >> 16;
            *bx++ = y & 0xffff;
#endif
#endif
        } while (sx <= sxe);
        bx = b->x;
        bxe = bx + n;
        if (!*bxe) {
            while (--bxe > bx && !*bxe)
                --n;
            b->wds = n;
        }
    }
    return q;
}

#ifndef MULTIPLE_THREADS
static char *dtoa_result;
#endif

#ifndef MULTIPLE_THREADS
static char *
rv_alloc(int i)
{
    return dtoa_result = xmalloc(i);
}
#else
#define rv_alloc(i) xmalloc(i)
#endif

static char *
nrv_alloc(const char *s, char **rve, size_t n)
{
    char *rv, *t;

    t = rv = rv_alloc(n);
    while ((*t = *s++) != 0) t++;
    if (rve)
        *rve = t;
    return rv;
}

#define rv_strdup(s, rve) nrv_alloc((s), (rve), strlen(s)+1)

#ifndef MULTIPLE_THREADS
/* freedtoa(s) must be used to free values s returned by dtoa
 * when MULTIPLE_THREADS is #defined.  It should be used in all cases,
 * but for consistency with earlier versions of dtoa, it is optional
 * when MULTIPLE_THREADS is not defined.
 */

static void
freedtoa(char *s)
{
    xfree(s);
}
#endif

static const char INFSTR[] = "Infinity";
static const char NANSTR[] = "NaN";
static const char ZEROSTR[] = "0";

/* dtoa for IEEE arithmetic (dmg): convert double to ASCII string.
 *
 * Inspired by "How to Print Floating-Point Numbers Accurately" by
 * Guy L. Steele, Jr. and Jon L. White [Proc. ACM SIGPLAN '90, pp. 112-126].
 *
 * Modifications:
 *  1. Rather than iterating, we use a simple numeric overestimate
 *     to determine k = floor(log10(d)).  We scale relevant
 *     quantities using O(log2(k)) rather than O(k) multiplications.
 *  2. For some modes > 2 (corresponding to ecvt and fcvt), we don't
 *     try to generate digits strictly left to right.  Instead, we
 *     compute with fewer bits and propagate the carry if necessary
 *     when rounding the final digit up.  This is often faster.
 *  3. Under the assumption that input will be rounded nearest,
 *     mode 0 renders 1e23 as 1e23 rather than 9.999999999999999e22.
 *     That is, we allow equality in stopping tests when the
 *     round-nearest rule will give the same floating-point value
 *     as would satisfaction of the stopping test with strict
 *     inequality.
 *  4. We remove common factors of powers of 2 from relevant
 *     quantities.
 *  5. When converting floating-point integers less than 1e16,
 *     we use floating-point arithmetic rather than resorting
 *     to multiple-precision integers.
 *  6. When asked to produce fewer than 15 digits, we first try
 *     to get by with floating-point arithmetic; we resort to
 *     multiple-precision integer arithmetic only if we cannot
 *     guarantee that the floating-point calculation has given
 *     the correctly rounded result.  For k requested digits and
 *     "uniformly" distributed input, the probability is
 *     something like 10^(k-15) that we must resort to the Long
 *     calculation.
 */

char *
ruby_dtoa(double d_, int mode, int ndigits, int *decpt, int *sign, char **rve)
{
 /* Arguments ndigits, decpt, sign are similar to those
    of ecvt and fcvt; trailing zeros are suppressed from
    the returned string.  If not null, *rve is set to point
    to the end of the return value.  If d is +-Infinity or NaN,
    then *decpt is set to 9999.

    mode:
        0 ==> shortest string that yields d when read in
            and rounded to nearest.
        1 ==> like 0, but with Steele & White stopping rule;
            e.g. with IEEE P754 arithmetic , mode 0 gives
            1e23 whereas mode 1 gives 9.999999999999999e22.
        2 ==> max(1,ndigits) significant digits.  This gives a
            return value similar to that of ecvt, except
            that trailing zeros are suppressed.
        3 ==> through ndigits past the decimal point.  This
            gives a return value similar to that from fcvt,
            except that trailing zeros are suppressed, and
            ndigits can be negative.
        4,5 ==> similar to 2 and 3, respectively, but (in
            round-nearest mode) with the tests of mode 0 to
            possibly return a shorter string that rounds to d.
            With IEEE arithmetic and compilation with
            -DHonor_FLT_ROUNDS, modes 4 and 5 behave the same
            as modes 2 and 3 when FLT_ROUNDS != 1.
        6-9 ==> Debugging modes similar to mode - 4:  don't try
            fast floating-point estimate (if applicable).

        Values of mode other than 0-9 are treated as mode 0.

        Sufficient space is allocated to the return value
        to hold the suppressed trailing zeros.
    */

    int bbits, b2, b5, be, dig, i, ieps, ilim, ilim0, ilim1,
        j, j1, k, k0, k_check, leftright, m2, m5, s2, s5,
        spec_case, try_quick;
    Long L;
#ifndef Sudden_Underflow
    int denorm;
    ULong x;
#endif
    Bigint *b, *b1, *delta, *mlo = 0, *mhi = 0, *S;
    double ds;
    double_u d, d2, eps;
    char *s, *s0;
#ifdef Honor_FLT_ROUNDS
    int rounding;
#endif
#ifdef SET_INEXACT
    int inexact, oldinexact;
#endif

    dval(d) = d_;

#ifndef MULTIPLE_THREADS
    if (dtoa_result) {
        freedtoa(dtoa_result);
        dtoa_result = 0;
    }
#endif

    if (word0(d) & Sign_bit) {
        /* set sign for everything, including 0's and NaNs */
        *sign = 1;
        word0(d) &= ~Sign_bit;  /* clear sign bit */
    }
    else
        *sign = 0;

#if defined(IEEE_Arith) + defined(VAX)
#ifdef IEEE_Arith
    if ((word0(d) & Exp_mask) == Exp_mask)
#else
    if (word0(d)  == 0x8000)
#endif
    {
        /* Infinity or NaN */
        *decpt = 9999;
#ifdef IEEE_Arith
        if (!word1(d) && !(word0(d) & 0xfffff))
            return rv_strdup(INFSTR, rve);
#endif
        return rv_strdup(NANSTR, rve);
    }
#endif
#ifdef IBM
    dval(d) += 0; /* normalize */
#endif
    if (!dval(d)) {
        *decpt = 1;
        return rv_strdup(ZEROSTR, rve);
    }

#ifdef SET_INEXACT
    try_quick = oldinexact = get_inexact();
    inexact = 1;
#endif
#ifdef Honor_FLT_ROUNDS
    if ((rounding = Flt_Rounds) >= 2) {
        if (*sign)
            rounding = rounding == 2 ? 0 : 2;
        else
            if (rounding != 2)
                rounding = 0;
    }
#endif

    b = d2b(dval(d), &be, &bbits);
#ifdef Sudden_Underflow
    i = (int)(word0(d) >> Exp_shift1 & (Exp_mask>>Exp_shift1));
#else
    if ((i = (int)(word0(d) >> Exp_shift1 & (Exp_mask>>Exp_shift1))) != 0) {
#endif
        dval(d2) = dval(d);
        word0(d2) &= Frac_mask1;
        word0(d2) |= Exp_11;
#ifdef IBM
        if (j = 11 - hi0bits(word0(d2) & Frac_mask))
            dval(d2) /= 1 << j;
#endif

        /* log(x)   ~=~ log(1.5) + (x-1.5)/1.5
         * log10(x)  =  log(x) / log(10)
         *      ~=~ log(1.5)/log(10) + (x-1.5)/(1.5*log(10))
         * log10(d) = (i-Bias)*log(2)/log(10) + log10(d2)
         *
         * This suggests computing an approximation k to log10(d) by
         *
         * k = (i - Bias)*0.301029995663981
         *  + ( (d2-1.5)*0.289529654602168 + 0.176091259055681 );
         *
         * We want k to be too large rather than too small.
         * The error in the first-order Taylor series approximation
         * is in our favor, so we just round up the constant enough
         * to compensate for any error in the multiplication of
         * (i - Bias) by 0.301029995663981; since |i - Bias| <= 1077,
         * and 1077 * 0.30103 * 2^-52 ~=~ 7.2e-14,
         * adding 1e-13 to the constant term more than suffices.
         * Hence we adjust the constant term to 0.1760912590558.
         * (We could get a more accurate k by invoking log10,
         *  but this is probably not worthwhile.)
         */

        i -= Bias;
#ifdef IBM
        i <<= 2;
        i += j;
#endif
#ifndef Sudden_Underflow
        denorm = 0;
    }
    else {
        /* d is denormalized */

        i = bbits + be + (Bias + (P-1) - 1);
        x = i > 32  ? word0(d) << (64 - i) | word1(d) >> (i - 32)
	    : word1(d) << (32 - i);
        dval(d2) = x;
        word0(d2) -= 31*Exp_msk1; /* adjust exponent */
        i -= (Bias + (P-1) - 1) + 1;
        denorm = 1;
    }
#endif
    ds = (dval(d2)-1.5)*0.289529654602168 + 0.1760912590558 + i*0.301029995663981;
    k = (int)ds;
    if (ds < 0. && ds != k)
        k--;    /* want k = floor(ds) */
    k_check = 1;
    if (k >= 0 && k <= Ten_pmax) {
        if (dval(d) < tens[k])
            k--;
        k_check = 0;
    }
    j = bbits - i - 1;
    if (j >= 0) {
        b2 = 0;
        s2 = j;
    }
    else {
        b2 = -j;
        s2 = 0;
    }
    if (k >= 0) {
        b5 = 0;
        s5 = k;
        s2 += k;
    }
    else {
        b2 -= k;
        b5 = -k;
        s5 = 0;
    }
    if (mode < 0 || mode > 9)
        mode = 0;

#ifndef SET_INEXACT
#ifdef Check_FLT_ROUNDS
    try_quick = Rounding == 1;
#else
    try_quick = 1;
#endif
#endif /*SET_INEXACT*/

    if (mode > 5) {
        mode -= 4;
        try_quick = 0;
    }
    leftright = 1;
    ilim = ilim1 = -1;
    switch (mode) {
      case 0:
      case 1:
        i = 18;
        ndigits = 0;
        break;
      case 2:
        leftright = 0;
        /* no break */
      case 4:
        if (ndigits <= 0)
            ndigits = 1;
        ilim = ilim1 = i = ndigits;
        break;
      case 3:
        leftright = 0;
        /* no break */
      case 5:
        i = ndigits + k + 1;
        ilim = i;
        ilim1 = i - 1;
        if (i <= 0)
            i = 1;
    }
    s = s0 = rv_alloc(i+1);

#ifdef Honor_FLT_ROUNDS
    if (mode > 1 && rounding != 1)
        leftright = 0;
#endif

    if (ilim >= 0 && ilim <= Quick_max && try_quick) {

        /* Try to get by with floating-point arithmetic. */

        i = 0;
        dval(d2) = dval(d);
        k0 = k;
        ilim0 = ilim;
        ieps = 2; /* conservative */
        if (k > 0) {
            ds = tens[k&0xf];
            j = k >> 4;
            if (j & Bletch) {
                /* prevent overflows */
                j &= Bletch - 1;
                dval(d) /= bigtens[n_bigtens-1];
                ieps++;
            }
            for (; j; j >>= 1, i++)
                if (j & 1) {
                    ieps++;
                    ds *= bigtens[i];
                }
            dval(d) /= ds;
        }
        else if ((j1 = -k) != 0) {
            dval(d) *= tens[j1 & 0xf];
            for (j = j1 >> 4; j; j >>= 1, i++)
                if (j & 1) {
                    ieps++;
                    dval(d) *= bigtens[i];
                }
        }
        if (k_check && dval(d) < 1. && ilim > 0) {
            if (ilim1 <= 0)
                goto fast_failed;
            ilim = ilim1;
            k--;
            dval(d) *= 10.;
            ieps++;
        }
        dval(eps) = ieps*dval(d) + 7.;
        word0(eps) -= (P-1)*Exp_msk1;
        if (ilim == 0) {
            S = mhi = 0;
            dval(d) -= 5.;
            if (dval(d) > dval(eps))
                goto one_digit;
            if (dval(d) < -dval(eps))
                goto no_digits;
            goto fast_failed;
        }
#ifndef No_leftright
        if (leftright) {
            /* Use Steele & White method of only
             * generating digits needed.
             */
            dval(eps) = 0.5/tens[ilim-1] - dval(eps);
            for (i = 0;;) {
                L = (int)dval(d);
                dval(d) -= L;
                *s++ = '0' + (int)L;
                if (dval(d) < dval(eps))
                    goto ret1;
                if (1. - dval(d) < dval(eps))
                    goto bump_up;
                if (++i >= ilim)
                    break;
                dval(eps) *= 10.;
                dval(d) *= 10.;
            }
        }
        else {
#endif
            /* Generate ilim digits, then fix them up. */
            dval(eps) *= tens[ilim-1];
            for (i = 1;; i++, dval(d) *= 10.) {
                L = (Long)(dval(d));
                if (!(dval(d) -= L))
                    ilim = i;
                *s++ = '0' + (int)L;
                if (i == ilim) {
                    if (dval(d) > 0.5 + dval(eps))
                        goto bump_up;
                    else if (dval(d) < 0.5 - dval(eps)) {
                        while (*--s == '0') ;
                        s++;
                        goto ret1;
                    }
                    break;
                }
            }
#ifndef No_leftright
        }
#endif
fast_failed:
        s = s0;
        dval(d) = dval(d2);
        k = k0;
        ilim = ilim0;
    }

    /* Do we have a "small" integer? */

    if (be >= 0 && k <= Int_max) {
        /* Yes. */
        ds = tens[k];
        if (ndigits < 0 && ilim <= 0) {
            S = mhi = 0;
            if (ilim < 0 || dval(d) <= 5*ds)
                goto no_digits;
            goto one_digit;
        }
        for (i = 1;; i++, dval(d) *= 10.) {
            L = (Long)(dval(d) / ds);
            dval(d) -= L*ds;
#ifdef Check_FLT_ROUNDS
            /* If FLT_ROUNDS == 2, L will usually be high by 1 */
            if (dval(d) < 0) {
                L--;
                dval(d) += ds;
            }
#endif
            *s++ = '0' + (int)L;
            if (!dval(d)) {
#ifdef SET_INEXACT
                inexact = 0;
#endif
                break;
            }
            if (i == ilim) {
#ifdef Honor_FLT_ROUNDS
                if (mode > 1)
                switch (rounding) {
                  case 0: goto ret1;
                  case 2: goto bump_up;
                }
#endif
                dval(d) += dval(d);
                if (dval(d) > ds || (dval(d) == ds && (L & 1))) {
bump_up:
                    while (*--s == '9')
                        if (s == s0) {
                            k++;
                            *s = '0';
                            break;
                        }
                    ++*s++;
                }
                break;
            }
        }
        goto ret1;
    }

    m2 = b2;
    m5 = b5;
    if (leftright) {
        i =
#ifndef Sudden_Underflow
            denorm ? be + (Bias + (P-1) - 1 + 1) :
#endif
#ifdef IBM
            1 + 4*P - 3 - bbits + ((bbits + be - 1) & 3);
#else
            1 + P - bbits;
#endif
        b2 += i;
        s2 += i;
        mhi = i2b(1);
    }
    if (m2 > 0 && s2 > 0) {
        i = m2 < s2 ? m2 : s2;
        b2 -= i;
        m2 -= i;
        s2 -= i;
    }
    if (b5 > 0) {
        if (leftright) {
            if (m5 > 0) {
                mhi = pow5mult(mhi, m5);
                b1 = mult(mhi, b);
                Bfree(b);
                b = b1;
            }
            if ((j = b5 - m5) != 0)
                b = pow5mult(b, j);
        }
        else
            b = pow5mult(b, b5);
    }
    S = i2b(1);
    if (s5 > 0)
        S = pow5mult(S, s5);

    /* Check for special case that d is a normalized power of 2. */

    spec_case = 0;
    if ((mode < 2 || leftright)
#ifdef Honor_FLT_ROUNDS
            && rounding == 1
#endif
    ) {
        if (!word1(d) && !(word0(d) & Bndry_mask)
#ifndef Sudden_Underflow
            && word0(d) & (Exp_mask & ~Exp_msk1)
#endif
        ) {
            /* The special case */
            b2 += Log2P;
            s2 += Log2P;
            spec_case = 1;
        }
    }

    /* Arrange for convenient computation of quotients:
     * shift left if necessary so divisor has 4 leading 0 bits.
     *
     * Perhaps we should just compute leading 28 bits of S once
     * and for all and pass them and a shift to quorem, so it
     * can do shifts and ors to compute the numerator for q.
     */
#ifdef Pack_32
    if ((i = ((s5 ? 32 - hi0bits(S->x[S->wds-1]) : 1) + s2) & 0x1f) != 0)
        i = 32 - i;
#else
    if ((i = ((s5 ? 32 - hi0bits(S->x[S->wds-1]) : 1) + s2) & 0xf) != 0)
        i = 16 - i;
#endif
    if (i > 4) {
        i -= 4;
        b2 += i;
        m2 += i;
        s2 += i;
    }
    else if (i < 4) {
        i += 28;
        b2 += i;
        m2 += i;
        s2 += i;
    }
    if (b2 > 0)
        b = lshift(b, b2);
    if (s2 > 0)
        S = lshift(S, s2);
    if (k_check) {
        if (bicmp(b,S) < 0) {
            k--;
            b = multadd(b, 10, 0);  /* we botched the k estimate */
            if (leftright)
                mhi = multadd(mhi, 10, 0);
            ilim = ilim1;
        }
    }
    if (ilim <= 0 && (mode == 3 || mode == 5)) {
        if (ilim < 0 || bicmp(b,S = multadd(S,5,0)) <= 0) {
            /* no digits, fcvt style */
no_digits:
            k = -1 - ndigits;
            goto ret;
        }
one_digit:
        *s++ = '1';
        k++;
        goto ret;
    }
    if (leftright) {
        if (m2 > 0)
            mhi = lshift(mhi, m2);

        /* Compute mlo -- check for special case
         * that d is a normalized power of 2.
         */

        mlo = mhi;
        if (spec_case) {
            mhi = Balloc(mhi->k);
            Bcopy(mhi, mlo);
            mhi = lshift(mhi, Log2P);
        }

        for (i = 1;;i++) {
            dig = quorem(b,S) + '0';
            /* Do we yet have the shortest decimal string
             * that will round to d?
             */
            j = bicmp(b, mlo);
            delta = diff(S, mhi);
            j1 = delta->sign ? 1 : bicmp(b, delta);
            Bfree(delta);
#ifndef ROUND_BIASED
            if (j1 == 0 && mode != 1 && !(word1(d) & 1)
#ifdef Honor_FLT_ROUNDS
                && rounding >= 1
#endif
            ) {
                if (dig == '9')
                    goto round_9_up;
                if (j > 0)
                    dig++;
#ifdef SET_INEXACT
                else if (!b->x[0] && b->wds <= 1)
                    inexact = 0;
#endif
                *s++ = dig;
                goto ret;
            }
#endif
            if (j < 0 || (j == 0 && mode != 1
#ifndef ROUND_BIASED
                && !(word1(d) & 1)
#endif
            )) {
                if (!b->x[0] && b->wds <= 1) {
#ifdef SET_INEXACT
                    inexact = 0;
#endif
                    goto accept_dig;
                }
#ifdef Honor_FLT_ROUNDS
                if (mode > 1)
                    switch (rounding) {
                      case 0: goto accept_dig;
                      case 2: goto keep_dig;
                    }
#endif /*Honor_FLT_ROUNDS*/
                if (j1 > 0) {
                    b = lshift(b, 1);
                    j1 = bicmp(b, S);
                    if ((j1 > 0 || (j1 == 0 && (dig & 1))) && dig++ == '9')
                        goto round_9_up;
                }
accept_dig:
                *s++ = dig;
                goto ret;
            }
            if (j1 > 0) {
#ifdef Honor_FLT_ROUNDS
                if (!rounding)
                    goto accept_dig;
#endif
                if (dig == '9') { /* possible if i == 1 */
round_9_up:
                    *s++ = '9';
                    goto roundoff;
                }
                *s++ = dig + 1;
                goto ret;
            }
#ifdef Honor_FLT_ROUNDS
keep_dig:
#endif
            *s++ = dig;
            if (i == ilim)
                break;
            b = multadd(b, 10, 0);
            if (mlo == mhi)
                mlo = mhi = multadd(mhi, 10, 0);
            else {
                mlo = multadd(mlo, 10, 0);
                mhi = multadd(mhi, 10, 0);
            }
        }
    }
    else
        for (i = 1;; i++) {
            *s++ = dig = quorem(b,S) + '0';
            if (!b->x[0] && b->wds <= 1) {
#ifdef SET_INEXACT
                inexact = 0;
#endif
                goto ret;
            }
            if (i >= ilim)
                break;
            b = multadd(b, 10, 0);
        }

    /* Round off last digit */

#ifdef Honor_FLT_ROUNDS
    switch (rounding) {
      case 0: goto trimzeros;
      case 2: goto roundoff;
    }
#endif
    b = lshift(b, 1);
    j = bicmp(b, S);
    if (j > 0 || (j == 0 && (dig & 1))) {
 roundoff:
        while (*--s == '9')
            if (s == s0) {
                k++;
                *s++ = '1';
                goto ret;
            }
        ++*s++;
    }
    else {
        while (*--s == '0') ;
        s++;
    }
ret:
    Bfree(S);
    if (mhi) {
        if (mlo && mlo != mhi)
            Bfree(mlo);
        Bfree(mhi);
    }
ret1:
#ifdef SET_INEXACT
    if (inexact) {
        if (!oldinexact) {
            word0(d) = Exp_1 + (70 << Exp_shift);
            word1(d) = 0;
            dval(d) += 1.;
        }
    }
    else if (!oldinexact)
        clear_inexact();
#endif
    Bfree(b);
    *s = 0;
    *decpt = k + 1;
    if (rve)
        *rve = s;
    return s0;
}

/*
 * copied from bignum.c
 */

#define BDIGITS(x) (RBIGNUM_DIGITS(x))

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
    st_index_t hash;

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
    char *ruby_dtoa(double d_, int mode, int ndigits, int *decpt, int *sign, char **rve);
    enum {decimal_mant = DBL_MANT_DIG-DBL_DIG};
    enum {float_dig = DBL_DIG+1};
    char buf[float_dig + (decimal_mant + CHAR_BIT - 1) / CHAR_BIT + 10];
    double value = RFLOAT_VALUE(flt);
    VALUE s;
    char *p, *e;
    int sign, decpt, digs;

    if (isinf(value))
	return rb_usascii_str_new2(value < 0 ? "-Infinity" : "Infinity");
    else if (isnan(value))
	return rb_usascii_str_new2("NaN");

    p = ruby_dtoa(value, 0, 0, &decpt, &sign, &e);
    s = sign ? rb_usascii_str_new_cstr("-") : rb_usascii_str_new(0, 0);
    if ((digs = (int)(e - p)) >= (int)sizeof(buf)) digs = (int)sizeof(buf) - 1;
    memcpy(buf, p, digs);
    xfree(p);
    if (decpt > 0) {
	if (decpt < digs) {
	    memmove(buf + decpt + 1, buf + decpt, digs - decpt);
	    buf[decpt] = '.';
	    rb_str_cat(s, buf, digs + 1);
	}
	else if (decpt <= DBL_DIG) {
	    long len;
	    char *ptr;
	    rb_str_cat(s, buf, digs);
	    rb_str_resize(s, (len = RSTRING_LEN(s)) + decpt - digs + 2);
	    ptr = RSTRING_PTR(s) + len;
	    if (decpt > digs) {
		memset(ptr, '0', decpt - digs);
		ptr += decpt - digs;
	    }
	    memcpy(ptr, ".0", 2);
	}
	else {
	    goto exp;
	}
    }
    else if (decpt > -4) {
	long len;
	char *ptr;
	rb_str_cat(s, "0.", 2);
	rb_str_resize(s, (len = RSTRING_LEN(s)) - decpt + digs);
	ptr = RSTRING_PTR(s);
	memset(ptr += len, '0', -decpt);
	memcpy(ptr -= decpt, buf, digs);
    }
    else {
      exp:
	if (digs > 1) {
	    memmove(buf + 2, buf + 1, digs - 1);
	}
	else {
	    buf[2] = '0';
	    digs++;
	}
	buf[1] = '.';
	rb_str_cat(s, buf, digs + 1);
	rb_str_catf(s, "e%+03d", decpt - 1);
    }
    return s;
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
	if (a == 0) return x;
        if (MUL_OVERFLOW_FIXNUM_P(a, b))
	    r = rb_big_mul(rb_int2big(a), rb_int2big(b));
        else
            r = LONG2FIX(a * b);
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
            if (MUL_OVERFLOW_FIXNUM_P(x, z)) {
		goto bignum;
	    }
	    z = x * z;
	}
    } while (--y);
    if (neg) z = -z;
    return LONG2NUM(z);
}

static VALUE fix_odd_p(VALUE num);

static VALUE
fix_pow(VALUE x, VALUE y)
{
    long a = FIX2LONG(x);

    if (FIXNUM_P(y)) {
	long b = FIX2LONG(y);

	if (a == 1) return INT2FIX(1);
	if (a == -1) {
	    if (b % 2 == 0)
		return INT2FIX(1);
	    else
		return INT2FIX(-1);
	}
	if (b < 0)
            rb_bug("fix_pow(): infinity returned");

	if (b == 0) return INT2FIX(1);
	if (b == 1) return x;
	if (a == 0) {
	    if (b > 0) return INT2FIX(0);
	    return DBL2NUM(INFINITY);
	}
	return int_pow(a, b);
    }
    /* modified */
    if (a == 1) return INT2FIX(1);
    if (a == -1) {
#define int_even_p(x) (FIXNUM_P(x) ? !fix_odd_p(x) : !rb_big_odd_p(x))
	if (int_even_p(y)) return INT2FIX(1);
	else return INT2FIX(-1);
    }
#define negative_int_p(x) (FIXNUM_P(x) ? (FIX2LONG(x) < 0) : !RBIGNUM_SIGN(x))
    if (negative_int_p(y))
	return rb_funcall(rb_rational_raw1(x), rb_intern("**"), 1, y);
    if (a == 0) return INT2FIX(0);
    x = rb_int2big(FIX2LONG(x));
    return rb_big_pow(x, y);
#undef int_even_p
#undef negative_int_p
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
