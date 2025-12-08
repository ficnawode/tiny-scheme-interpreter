#include "numbers.h"
#include "util.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))

static SchemeNum bn_normalize(SchemeNum num);
static SchemeNum bn_from_fix(int64_t val);
static SchemeNum bn_add_signed(SchemeNum a, SchemeNum b);
static SchemeNum bn_sub_signed(SchemeNum a, SchemeNum b);
static SchemeNum bn_mul_signed(SchemeNum a, SchemeNum b);

static SchemeNum add_complex(SchemeNum a, SchemeNum b);
static SchemeNum add_rational(SchemeNum a, SchemeNum b);
static SchemeNum add_integer(SchemeNum a, SchemeNum b);

static SchemeNum mul_complex(SchemeNum a, SchemeNum b);
static SchemeNum mul_rational(SchemeNum a, SchemeNum b);
static SchemeNum mul_integer(SchemeNum a, SchemeNum b);

static SchemeNum sub_complex(SchemeNum a, SchemeNum b);
static SchemeNum sub_rational(SchemeNum a, SchemeNum b);
static SchemeNum sub_integer(SchemeNum a, SchemeNum b);

static int64_t gcd_fix(int64_t a, int64_t b);

SchemeNum fixnum_create(int64_t val)
{
    SchemeNum num;
    num.type = NUM_FIXNUM;
    num.exact = true;
    num.value.fixnum = val;
    return num;
}

SchemeNum floatnum_create(double val)
{
    SchemeNum num;
    num.type = NUM_FLOAT;
    num.exact = false;
    num.value.floatnum = val;
    return num;
}

static SchemeNum bignum_alloc(size_t capacity)
{
    SchemeNum num;
    num.type = NUM_BIGNUM;
    num.exact = true;
    num.value.bignum = xmalloc(sizeof(BigNum));
    num.value.bignum->sign = 1;
    num.value.bignum->len = 0;
    num.value.bignum->capacity = capacity;
    num.value.bignum->limbs = xmalloc(capacity * sizeof(uint32_t));
    return num;
}

SchemeNum bignum_create_from_fix(int64_t val)
{
    return bn_from_fix(val);
}

SchemeNum rational_create(SchemeNum num, SchemeNum den)
{
    if (den.type == NUM_FIXNUM && den.value.fixnum == 0) {
        fprintf(stderr, "Error: Division by zero\n");
        exit(1);
    }
    // If den is 1, return num (Canonicalization)
    if (den.type == NUM_FIXNUM && den.value.fixnum == 1)
        return num;

    // Simplification (GCD) - Currently only for FixNums for brevity
    if (num.type == NUM_FIXNUM && den.type == NUM_FIXNUM) {
        int64_t n = num.value.fixnum;
        int64_t d = den.value.fixnum;
        if (d < 0) {
            n = -n;
            d = -d;
        }
        int64_t common = gcd_fix(n, d);
        n /= common;
        d /= common;
        if (d == 1)
            return fixnum_create(n);
        num = fixnum_create(n);
        den = fixnum_create(d);
    }

    SchemeNum res;
    res.type = NUM_RATIONAL;
    res.exact = num.exact && den.exact;
    res.value.rational = xmalloc(sizeof(Rational));
    res.value.rational->numerator = num;
    res.value.rational->denominator = den;
    return res;
}

SchemeNum complex_create(SchemeNum real, SchemeNum imag)
{
    // If imag is exact 0, return real (Canonicalization)
    bool is_zero = (imag.type == NUM_FIXNUM && imag.value.fixnum == 0)
        || (imag.type == NUM_FLOAT && imag.value.floatnum == 0.0);
    if (is_zero) {
        return real;
    }

    SchemeNum res;
    res.type = NUM_COMPLEX;
    res.exact = real.exact && imag.exact;
    res.value.complex_num = xmalloc(sizeof(ComplexNum));
    res.value.complex_num->real = real;
    res.value.complex_num->imag = imag;
    return res;
}

double to_double(SchemeNum n)
{
    switch (n.type) {
    case NUM_FIXNUM:
        return (double)n.value.fixnum;
    case NUM_FLOAT:
        return n.value.floatnum;
    case NUM_RATIONAL:
        return to_double(n.value.rational->numerator) / to_double(n.value.rational->denominator);
    case NUM_COMPLEX:
        return to_double(n.value.complex_num->real);
    case NUM_BIGNUM: {
        double res = 0;
        BigNum* bn = n.value.bignum;
        for (size_t i = 0; i < bn->len; i++) {
            res += (double)bn->limbs[i] * pow(2.0, 32.0 * i);
        }
        return res * bn->sign;
    }
    }
    return 0.0;
}

static int64_t gcd_fix(int64_t a, int64_t b)
{
    if (a < 0)
        a = -a;
    if (b < 0)
        b = -b;
    while (b != 0) {
        int64_t temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

static SchemeNum bn_normalize(SchemeNum num)
{
    BigNum* bn = num.value.bignum;
    while (bn->len > 0 && bn->limbs[bn->len - 1] == 0) {
        bn->len--;
    }
    if (bn->len == 0)
        return fixnum_create(0); // Zero

    // Try to demote to FixNum
    // Fits in 63 bits (signed 64-bit integer)?
    // 1 limb always fits. 2 limbs (64 bits) might fit if top bit is 0.
    if (bn->len == 1) {
        int64_t val = (int64_t)bn->limbs[0];
        return fixnum_create(bn->sign * val);
    }
    if (bn->len == 2) {
        uint64_t full = ((uint64_t)bn->limbs[1] << 32) | bn->limbs[0];
        if (full <= 0x7FFFFFFFFFFFFFFF) {
            return fixnum_create(bn->sign * (int64_t)full);
        }
    }

    return num;
}

static SchemeNum bn_from_fix(int64_t val)
{
    if (val == 0) {
        return bignum_alloc(0); // Empty bignum represents 0
    }
    SchemeNum num = bignum_alloc(2); // Fixnums fit in 2 limbs max

    num.value.bignum->sign = (val < 0) ? -1 : 1;
    uint64_t abs_val = (val < 0) ? -val : val;

    num.value.bignum->limbs[0] = (uint32_t)(abs_val & 0xFFFFFFFF);
    num.value.bignum->limbs[1] = (uint32_t)(abs_val >> 32);
    num.value.bignum->len = (num.value.bignum->limbs[1] > 0) ? 2 : 1;

    return num;
}

// Return -1 if |a| < |b|, 1 if |a| > |b|, 0 if equal
static int bn_cmp_mag(BigNum* a, BigNum* b)
{
    if (a->len != b->len)
        return (a->len < b->len) ? -1 : 1;
    for (size_t i = a->len; i > 0; i--) {
        if (a->limbs[i - 1] != b->limbs[i - 1])
            return (a->limbs[i - 1] < b->limbs[i - 1]) ? -1 : 1;
    }
    return 0;
}

// Magnitude Addition: result = |a| + |b|
static SchemeNum bn_add_mag(BigNum* a, BigNum* b)
{
    size_t max_len = MAX(a->len, b->len);
    SchemeNum res = bignum_alloc(max_len + 1);

    uint64_t carry = 0;
    for (size_t i = 0; i < max_len || carry; i++) {
        uint64_t sum = carry;
        if (i < a->len)
            sum += a->limbs[i];
        if (i < b->len)
            sum += b->limbs[i];

        res.value.bignum->limbs[i] = (uint32_t)(sum & 0xFFFFFFFF);
        carry = sum >> 32;
        res.value.bignum->len = i + 1;
    }
    return res;
}

// Magnitude Subtraction: result = |a| - |b|. ASSUMES |a| >= |b|
static SchemeNum bn_sub_mag(BigNum* a, BigNum* b)
{
    SchemeNum res = bignum_alloc(a->len);
    res.value.bignum->len = a->len;

    int64_t borrow = 0;
    for (size_t i = 0; i < a->len; i++) {
        int64_t diff = (int64_t)a->limbs[i] - borrow;
        if (i < b->len)
            diff -= b->limbs[i];

        if (diff < 0) {
            diff += 0x100000000LL; // 2^32
            borrow = 1;
        } else {
            borrow = 0;
        }
        res.value.bignum->limbs[i] = (uint32_t)diff;
    }
    // No normalization here, done by wrapper
    return res;
}

// Magnitude Multiplication: result = |a| * |b|
static SchemeNum bn_mul_mag(BigNum* a, BigNum* b)
{
    size_t len_res = a->len + b->len;
    SchemeNum res = bignum_alloc(len_res);
    res.value.bignum->len = len_res; // Temporarily set full len

    for (size_t i = 0; i < a->len; i++) {
        uint64_t carry = 0;
        for (size_t j = 0; j < b->len; j++) {
            uint64_t prod = (uint64_t)a->limbs[i] * b->limbs[j]
                + res.value.bignum->limbs[i + j]
                + carry;
            res.value.bignum->limbs[i + j] = (uint32_t)(prod & 0xFFFFFFFF);
            carry = prod >> 32;
        }
        res.value.bignum->limbs[i + b->len] += (uint32_t)carry;
    }
    return res;
}

static SchemeNum bn_add_signed(SchemeNum a, SchemeNum b)
{
    // If signs match, add magnitudes and keep sign
    if (a.value.bignum->sign == b.value.bignum->sign) {
        SchemeNum res = bn_add_mag(a.value.bignum, b.value.bignum);
        res.value.bignum->sign = a.value.bignum->sign;
        return bn_normalize(res);
    }

    // Signs differ: Subtract smaller mag from larger mag
    int cmp = bn_cmp_mag(a.value.bignum, b.value.bignum);
    if (cmp == 0)
        return fixnum_create(0); // Cancel out

    SchemeNum res;
    if (cmp > 0) { // |a| > |b|, keep a's sign
        res = bn_sub_mag(a.value.bignum, b.value.bignum);
        res.value.bignum->sign = a.value.bignum->sign;
    } else { // |b| > |a|, keep b's sign
        res = bn_sub_mag(b.value.bignum, a.value.bignum);
        res.value.bignum->sign = b.value.bignum->sign;
    }
    return bn_normalize(res);
}

static SchemeNum bn_sub_signed(SchemeNum a, SchemeNum b)
{
    // a - b is same as a + (-b)
    // Create a temporary alias for b with flipped sign (careful with memory in real impl)
    // To be safe, we'll just clone logical op:

    // If signs differ (e.g. 5 - (-3)), it becomes addition (5 + 3)
    if (a.value.bignum->sign != b.value.bignum->sign) {
        SchemeNum res = bn_add_mag(a.value.bignum, b.value.bignum);
        res.value.bignum->sign = a.value.bignum->sign;
        return bn_normalize(res);
    }

    // Signs match (e.g. 5 - 3 or -5 - (-3)) -> Subtract magnitudes
    int cmp = bn_cmp_mag(a.value.bignum, b.value.bignum);
    if (cmp == 0)
        return fixnum_create(0);

    SchemeNum res;
    if (cmp > 0) { // |a| > |b|
        res = bn_sub_mag(a.value.bignum, b.value.bignum);
        res.value.bignum->sign = a.value.bignum->sign;
    } else { // |b| > |a|
        res = bn_sub_mag(b.value.bignum, a.value.bignum);
        res.value.bignum->sign = -a.value.bignum->sign; // Flip a's sign
    }
    return bn_normalize(res);
}

static SchemeNum bn_mul_signed(SchemeNum a, SchemeNum b)
{
    SchemeNum res = bn_mul_mag(a.value.bignum, b.value.bignum);
    res.value.bignum->sign = a.value.bignum->sign * b.value.bignum->sign;
    return bn_normalize(res);
}

SchemeNum scheme_add(SchemeNum a, SchemeNum b)
{
    if (a.type == NUM_COMPLEX || b.type == NUM_COMPLEX)
        return add_complex(a, b);
    if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
        return floatnum_create(to_double(a) + to_double(b));
    if (a.type == NUM_RATIONAL || b.type == NUM_RATIONAL)
        return add_rational(a, b);
    return add_integer(a, b);
}

static SchemeNum add_complex(SchemeNum a, SchemeNum b)
{
    SchemeNum r1 = (a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
    SchemeNum i1 = (a.type == NUM_COMPLEX) ? a.value.complex_num->imag : fixnum_create(0);
    SchemeNum r2 = (b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
    SchemeNum i2 = (b.type == NUM_COMPLEX) ? b.value.complex_num->imag : fixnum_create(0);
    return complex_create(scheme_add(r1, r2), scheme_add(i1, i2));
}

static SchemeNum add_rational(SchemeNum a, SchemeNum b)
{
    SchemeNum n1 = (a.type == NUM_RATIONAL) ? a.value.rational->numerator : a;
    SchemeNum d1 = (a.type == NUM_RATIONAL) ? a.value.rational->denominator : fixnum_create(1);
    SchemeNum n2 = (b.type == NUM_RATIONAL) ? b.value.rational->numerator : b;
    SchemeNum d2 = (b.type == NUM_RATIONAL) ? b.value.rational->denominator : fixnum_create(1);

    // (n1*d2 + n2*d1) / (d1*d2)
    SchemeNum top = scheme_add(scheme_mul(n1, d2), scheme_mul(n2, d1));
    SchemeNum bot = scheme_mul(d1, d2);
    return rational_create(top, bot);
}

static SchemeNum add_integer(SchemeNum a, SchemeNum b)
{
    // Fast path: Fixnum + Fixnum
    if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM) {
        int64_t res;
        if (!__builtin_add_overflow(a.value.fixnum, b.value.fixnum, &res)) {
            return fixnum_create(res);
        }
    }
    // Slow path: Promote both to BigNum, add, then normalize
    SchemeNum ba = (a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
    SchemeNum bb = (b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;
    return bn_add_signed(ba, bb);
}

SchemeNum scheme_sub(SchemeNum a, SchemeNum b)
{
    if (a.type == NUM_COMPLEX || b.type == NUM_COMPLEX)
        return sub_complex(a, b);
    if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
        return floatnum_create(to_double(a) - to_double(b));
    if (a.type == NUM_RATIONAL || b.type == NUM_RATIONAL)
        return sub_rational(a, b);
    return sub_integer(a, b);
}

static SchemeNum sub_complex(SchemeNum a, SchemeNum b)
{
    SchemeNum r1 = (a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
    SchemeNum i1 = (a.type == NUM_COMPLEX) ? a.value.complex_num->imag : fixnum_create(0);
    SchemeNum r2 = (b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
    SchemeNum i2 = (b.type == NUM_COMPLEX) ? b.value.complex_num->imag : fixnum_create(0);
    return complex_create(scheme_sub(r1, r2), scheme_sub(i1, i2));
}

static SchemeNum sub_rational(SchemeNum a, SchemeNum b)
{
    SchemeNum n1 = (a.type == NUM_RATIONAL) ? a.value.rational->numerator : a;
    SchemeNum d1 = (a.type == NUM_RATIONAL) ? a.value.rational->denominator : fixnum_create(1);
    SchemeNum n2 = (b.type == NUM_RATIONAL) ? b.value.rational->numerator : b;
    SchemeNum d2 = (b.type == NUM_RATIONAL) ? b.value.rational->denominator : fixnum_create(1);

    // (n1*d2 - n2*d1) / (d1*d2)
    SchemeNum top = scheme_sub(scheme_mul(n1, d2), scheme_mul(n2, d1));
    SchemeNum bot = scheme_mul(d1, d2);
    return rational_create(top, bot);
}

static SchemeNum sub_integer(SchemeNum a, SchemeNum b)
{
    if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM) {
        int64_t res;
        if (!__builtin_sub_overflow(a.value.fixnum, b.value.fixnum, &res)) {
            return fixnum_create(res);
        }
    }
    SchemeNum ba = (a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
    SchemeNum bb = (b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;
    return bn_sub_signed(ba, bb);
}

SchemeNum scheme_mul(SchemeNum a, SchemeNum b)
{
    if (a.type == NUM_COMPLEX || b.type == NUM_COMPLEX)
        return mul_complex(a, b);
    if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
        return floatnum_create(to_double(a) * to_double(b));
    if (a.type == NUM_RATIONAL || b.type == NUM_RATIONAL)
        return mul_rational(a, b);
    return mul_integer(a, b);
}

static SchemeNum mul_complex(SchemeNum a, SchemeNum b)
{
    SchemeNum ar = (a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
    SchemeNum ai = (a.type == NUM_COMPLEX) ? a.value.complex_num->imag : fixnum_create(0);
    SchemeNum br = (b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
    SchemeNum bi = (b.type == NUM_COMPLEX) ? b.value.complex_num->imag : fixnum_create(0);

    // (ar*br - ai*bi) + (ar*bi + ai*br)i
    SchemeNum re = scheme_sub(scheme_mul(ar, br), scheme_mul(ai, bi));
    SchemeNum im = scheme_add(scheme_mul(ar, bi), scheme_mul(ai, br));
    return complex_create(re, im);
}

static SchemeNum mul_rational(SchemeNum a, SchemeNum b)
{
    SchemeNum n1 = (a.type == NUM_RATIONAL) ? a.value.rational->numerator : a;
    SchemeNum d1 = (a.type == NUM_RATIONAL) ? a.value.rational->denominator : fixnum_create(1);
    SchemeNum n2 = (b.type == NUM_RATIONAL) ? b.value.rational->numerator : b;
    SchemeNum d2 = (b.type == NUM_RATIONAL) ? b.value.rational->denominator : fixnum_create(1);
    return rational_create(scheme_mul(n1, n2), scheme_mul(d1, d2));
}

static SchemeNum mul_integer(SchemeNum a, SchemeNum b)
{
    if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM) {
        int64_t res;
        if (!__builtin_mul_overflow(a.value.fixnum, b.value.fixnum, &res)) {
            return fixnum_create(res);
        }
    }
    SchemeNum ba = (a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
    SchemeNum bb = (b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;
    return bn_mul_signed(ba, bb);
}
