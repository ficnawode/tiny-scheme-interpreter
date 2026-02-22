#include "numbers.h"
#include "util.h"

#include <inttypes.h>
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
static bool bn_is_zero(BigNum *bn);

static SchemeNum add_complex(SchemeNum a, SchemeNum b);
static SchemeNum add_rational(SchemeNum a, SchemeNum b);
static SchemeNum add_integer(SchemeNum a, SchemeNum b);

static SchemeNum mul_complex(SchemeNum a, SchemeNum b);
static SchemeNum mul_rational(SchemeNum a, SchemeNum b);
static SchemeNum mul_integer(SchemeNum a, SchemeNum b);

static SchemeNum sub_complex(SchemeNum a, SchemeNum b);
static SchemeNum sub_rational(SchemeNum a, SchemeNum b);
static SchemeNum sub_integer(SchemeNum a, SchemeNum b);

static SchemeNum div_integer(SchemeNum a, SchemeNum b);
static SchemeNum div_rational(SchemeNum a, SchemeNum b);
static SchemeNum div_complex(SchemeNum a, SchemeNum b);

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
	if (den.type == NUM_FIXNUM && den.value.fixnum == 0)
	{
		fprintf(stderr, "Error: Division by zero\n");
		exit(1);
	}

	// Canonicalization
	if (den.type == NUM_FIXNUM && den.value.fixnum == 1)
		return num;

	// Simplification (GCD) - Currently only for FixNums for brevity
	if (num.type == NUM_FIXNUM && den.type == NUM_FIXNUM)
	{
		int64_t n = num.value.fixnum;
		int64_t d = den.value.fixnum;
		if (d < 0)
		{
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

bool schemenum_is_real(SchemeNum a)
{
	if (a.type == NUM_COMPLEX)
	{
		return schemenum_is_zero(a.value.complex_num->imag);
	}
	return true;
}

SchemeNum complex_create(SchemeNum real, SchemeNum imag)
{
	SchemeNum res;
	res.type = NUM_COMPLEX;
	res.exact = real.exact && imag.exact;
	res.value.complex_num = xmalloc(sizeof(ComplexNum));
	res.value.complex_num->real = real;
	res.value.complex_num->imag = imag;

	// Canonicalization
	if (schemenum_is_real(res))
	{
		schemenum_free(res);
		return real;
	}
	return res;
}

double to_double(SchemeNum n)
{
	switch (n.type)
	{
	case NUM_FIXNUM:
		return (double)n.value.fixnum;
	case NUM_FLOAT:
		return n.value.floatnum;
	case NUM_RATIONAL:
		return to_double(n.value.rational->numerator) /
			   to_double(n.value.rational->denominator);
	case NUM_COMPLEX:
		return to_double(n.value.complex_num->real);
	case NUM_BIGNUM:
	{
		double res = 0;
		BigNum *bn = n.value.bignum;
		for (size_t i = 0; i < bn->len; i++)
		{
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
	while (b != 0)
	{
		int64_t temp = b;
		b = a % b;
		a = temp;
	}
	return a;
}

static SchemeNum bn_normalize(SchemeNum num)
{
	BigNum *bn = num.value.bignum;
	while (bn->len > 0 && bn->limbs[bn->len - 1] == 0)
	{
		bn->len--;
	}
	if (bn->len == 0)
		return fixnum_create(0);

	// Try to demote to FixNum
	// Fits in 63 bits (signed 64-bit integer)?
	// 1 limb always fits.
	// 2 limbs (64 bits) might fit if top bit is 0.
	if (bn->len == 1)
	{
		int64_t val = (int64_t)bn->limbs[0];
		return fixnum_create(bn->sign * val);
	}
	if (bn->len == 2)
	{
		uint64_t full = ((uint64_t)bn->limbs[1] << 32) | bn->limbs[0];
		if (full <= 0x7FFFFFFFFFFFFFFF)
		{
			return fixnum_create(bn->sign * (int64_t)full);
		}
	}

	return num;
}

static SchemeNum bn_from_fix(int64_t val)
{
	if (val == 0)
	{
		return bignum_alloc(0);
	}
	SchemeNum num = bignum_alloc(2);

	num.value.bignum->sign = (val < 0) ? -1 : 1;
	uint64_t abs_val = (val < 0) ? -val : val;

	num.value.bignum->limbs[0] = (uint32_t)(abs_val & 0xFFFFFFFF);
	num.value.bignum->limbs[1] = (uint32_t)(abs_val >> 32);
	num.value.bignum->len = (num.value.bignum->limbs[1] > 0) ? 2 : 1;

	return num;
}

static int bn_num_bits(BigNum *bn)
{
	if (bn->len == 0)
		return 0;
	size_t limb_idx = bn->len - 1;
	uint32_t top = bn->limbs[limb_idx];
	int bits = 0;
	while (top > 0)
	{
		bits++;
		top >>= 1;
	}
	return (limb_idx * 32) + bits;
}

static int bn_get_bit(BigNum *bn, int bit)
{
	size_t limb_idx = bit / 32;
	int bit_idx = bit % 32;
	if (limb_idx >= bn->len)
		return 0;
	return (bn->limbs[limb_idx] >> bit_idx) & 1;
}

static void bn_set_bit(BigNum *bn, int bit)
{
	size_t limb_idx = bit / 32;
	int bit_idx = bit % 32;
	if (limb_idx >= bn->len)
	{
		for (size_t i = bn->len; i <= limb_idx; i++)
			bn->limbs[i] = 0;
		bn->len = limb_idx + 1;
	}
	bn->limbs[limb_idx] |= (1U << bit_idx);
}

// In-place: R = (R << 1) | bit
static void bn_lshift_insert(BigNum *bn, int bit)
{
	uint32_t carry = bit;
	for (size_t i = 0; i < bn->len; i++)
	{
		uint64_t val = ((uint64_t)bn->limbs[i] << 1) | carry;
		bn->limbs[i] = (uint32_t)(val & 0xFFFFFFFF);
		carry = (uint32_t)(val >> 32);
	}
	if (carry)
	{
		if (bn->len < bn->capacity)
		{
			bn->limbs[bn->len++] = carry;
		}
		else
		{
			size_t new_cap = bn->capacity * 2;
			bn->limbs =
				xrealloc(bn->limbs, new_cap * sizeof(uint32_t));
			bn->capacity = new_cap;
			bn->limbs[bn->len++] = carry;
		}
	}
}

// In-place: A = A - B (Assumes A >= B)
static void bn_sub_mag_inplace(BigNum *a, BigNum *b)
{
	int64_t borrow = 0;
	for (size_t i = 0; i < a->len; i++)
	{
		int64_t val_a = a->limbs[i];
		int64_t val_b = (i < b->len) ? b->limbs[i] : 0;
		int64_t diff = val_a - val_b - borrow;

		if (diff < 0)
		{
			diff += 0x100000000LL;
			borrow = 1;
		}
		else
		{
			borrow = 0;
		}
		a->limbs[i] = (uint32_t)diff;
	}
	// Normalize
	while (a->len > 0 && a->limbs[a->len - 1] == 0)
		a->len--;
}

// Return -1 if |a| < |b|, 1 if |a| > |b|, 0 if equal
static int bn_cmp_mag(BigNum *a, BigNum *b)
{
	if (a->len != b->len)
		return (a->len < b->len) ? -1 : 1;
	for (size_t i = a->len; i > 0; i--)
	{
		if (a->limbs[i - 1] != b->limbs[i - 1])
			return (a->limbs[i - 1] < b->limbs[i - 1]) ? -1 : 1;
	}
	return 0;
}

// Core Division Algorithm: Q = U / V, R = U % V
static void
bn_div_rem_mag(BigNum *u, BigNum *v, BigNum **q_out, BigNum **r_out)
{
	int cmp = bn_cmp_mag(u, v);
	if (cmp < 0)
	{
		// Q = 0, R = U
		SchemeNum z = bignum_alloc(1);
		z.value.bignum->len = 0;
		*q_out = z.value.bignum;

		SchemeNum r_copy = bignum_alloc(u->len > 0 ? u->len : 1);
		if (u->len > 0)
			memcpy(r_copy.value.bignum->limbs, u->limbs,
				   u->len * sizeof(uint32_t));
		r_copy.value.bignum->len = u->len;
		*r_out = r_copy.value.bignum;
		return;
	}

	int n_bits = bn_num_bits(u);

	SchemeNum q_sn = bignum_alloc((n_bits / 32) + 2);
	BigNum *q = q_sn.value.bignum;
	q->len = 0;

	SchemeNum r_sn = bignum_alloc(u->len + 2);
	BigNum *r = r_sn.value.bignum;
	r->len = 0;

	for (int i = n_bits - 1; i >= 0; i--)
	{
		int bit = bn_get_bit(u, i);
		bn_lshift_insert(r, bit);

		if (bn_cmp_mag(r, v) >= 0)
		{
			bn_sub_mag_inplace(r, v);
			bn_set_bit(q, i);
		}
	}
	*q_out = q;
	*r_out = r;
}

static void bn_div_rem(SchemeNum a,
					   SchemeNum b,
					   SchemeNum *q_res,
					   SchemeNum *r_res)
{
	SchemeNum aa =
		(a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
	SchemeNum bb =
		(b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;

	if (bb.value.bignum->len == 0)
	{
		*q_res = fixnum_create(0);
		*r_res = fixnum_create(0);
		return;
	}

	BigNum *q_bn, *r_bn;
	bn_div_rem_mag(aa.value.bignum, bb.value.bignum, &q_bn, &r_bn);

	// Set signs: Q.sign = A.sign * B.sign, R.sign = A.sign
	q_bn->sign = aa.value.bignum->sign * bb.value.bignum->sign;
	r_bn->sign = aa.value.bignum->sign;

	SchemeNum q_final = {
		.type = NUM_BIGNUM, .exact = true, .value.bignum = q_bn};
	SchemeNum r_final = {
		.type = NUM_BIGNUM, .exact = true, .value.bignum = r_bn};

	*q_res = bn_normalize(q_final);
	*r_res = bn_normalize(r_final);

	if (a.type == NUM_FIXNUM)
		schemenum_free(aa);
	if (b.type == NUM_FIXNUM)
		schemenum_free(bb);
}

static SchemeNum div_integer(SchemeNum a, SchemeNum b)
{
	if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM)
	{
		if (b.value.fixnum == 0)
			return floatnum_create(0.0);
		if (a.value.fixnum % b.value.fixnum == 0)
			return fixnum_create(a.value.fixnum / b.value.fixnum);
		return rational_create(a, b);
	}

	SchemeNum q, r;
	bn_div_rem(a, b, &q, &r);

	if (r.type == NUM_FIXNUM && r.value.fixnum == 0)
	{
		schemenum_free(r);
		return q;
	}

	schemenum_free(q);
	schemenum_free(r);
	return rational_create(a, b);
}

// Magnitude Addition: result = |a| + |b|
static SchemeNum bn_add_mag(BigNum *a, BigNum *b)
{
	size_t max_len = MAX(a->len, b->len);
	SchemeNum res = bignum_alloc(max_len + 1);

	uint64_t carry = 0;
	for (size_t i = 0; i < max_len || carry; i++)
	{
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
static SchemeNum bn_sub_mag(BigNum *a, BigNum *b)
{
	SchemeNum res = bignum_alloc(a->len);
	res.value.bignum->len = a->len;

	int64_t borrow = 0;
	for (size_t i = 0; i < a->len; i++)
	{
		int64_t diff = (int64_t)a->limbs[i] - borrow;
		if (i < b->len)
			diff -= b->limbs[i];

		if (diff < 0)
		{
			diff += 0x100000000LL; // 2^32
			borrow = 1;
		}
		else
		{
			borrow = 0;
		}
		res.value.bignum->limbs[i] = (uint32_t)diff;
	}
	return res;
}

// Magnitude Multiplication: |a| * |b|
static SchemeNum bn_mul_mag(BigNum *a, BigNum *b)
{
	size_t len_res = a->len + b->len;
	SchemeNum res = bignum_alloc(len_res);
	res.value.bignum->len = len_res;

	for (size_t i = 0; i < a->len; i++)
	{
		uint64_t carry = 0;
		for (size_t j = 0; j < b->len; j++)
		{
			uint64_t prod = (uint64_t)a->limbs[i] * b->limbs[j] +
							res.value.bignum->limbs[i + j] + carry;
			res.value.bignum->limbs[i + j] =
				(uint32_t)(prod & 0xFFFFFFFF);
			carry = prod >> 32;
		}
		res.value.bignum->limbs[i + b->len] += (uint32_t)carry;
	}
	return res;
}

static SchemeNum bn_add_signed(SchemeNum a, SchemeNum b)
{
	// If signs match, add magnitudes and keep sign
	if (a.value.bignum->sign == b.value.bignum->sign)
	{
		SchemeNum res = bn_add_mag(a.value.bignum, b.value.bignum);
		res.value.bignum->sign = a.value.bignum->sign;
		return bn_normalize(res);
	}

	// Signs differ: Subtract smaller mag from larger mag
	int cmp = bn_cmp_mag(a.value.bignum, b.value.bignum);
	if (cmp == 0)
		return fixnum_create(0); // Cancel out

	SchemeNum res;
	if (cmp > 0)
	{ // |a| > |b|, keep a's sign
		res = bn_sub_mag(a.value.bignum, b.value.bignum);
		res.value.bignum->sign = a.value.bignum->sign;
	}
	else
	{ // |b| > |a|, keep b's sign
		res = bn_sub_mag(b.value.bignum, a.value.bignum);
		res.value.bignum->sign = b.value.bignum->sign;
	}
	return bn_normalize(res);
}

static SchemeNum bn_sub_signed(SchemeNum a, SchemeNum b)
{
	// a - b == a + (-b)
	// If signs differ (e.g. 5 - (-3)), it becomes addition (5 + 3)
	if (a.value.bignum->sign != b.value.bignum->sign)
	{
		SchemeNum res = bn_add_mag(a.value.bignum, b.value.bignum);
		res.value.bignum->sign = a.value.bignum->sign;
		return bn_normalize(res);
	}

	// Signs match (e.g. 5 - 3 or -5 - (-3)) -> Subtract magnitudes
	int cmp = bn_cmp_mag(a.value.bignum, b.value.bignum);
	if (cmp == 0)
		return fixnum_create(0);

	SchemeNum res;
	if (cmp > 0)
	{ // |a| > |b|
		res = bn_sub_mag(a.value.bignum, b.value.bignum);
		res.value.bignum->sign = a.value.bignum->sign;
	}
	else
	{ // |b| > |a|
		res = bn_sub_mag(b.value.bignum, a.value.bignum);
		res.value.bignum->sign = -a.value.bignum->sign;
	}
	return bn_normalize(res);
}

static SchemeNum bn_mul_signed(SchemeNum a, SchemeNum b)
{
	SchemeNum res = bn_mul_mag(a.value.bignum, b.value.bignum);
	res.value.bignum->sign =
		a.value.bignum->sign * b.value.bignum->sign;
	return bn_normalize(res);
}

SchemeNum schemenum_add(SchemeNum a, SchemeNum b)
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
	SchemeNum r1 =
		(a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
	SchemeNum i1 = (a.type == NUM_COMPLEX) ? a.value.complex_num->imag
										   : fixnum_create(0);
	SchemeNum r2 =
		(b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
	SchemeNum i2 = (b.type == NUM_COMPLEX) ? b.value.complex_num->imag
										   : fixnum_create(0);
	return complex_create(schemenum_add(r1, r2),
						  schemenum_add(i1, i2));
}

static SchemeNum add_rational(SchemeNum a, SchemeNum b)
{
	SchemeNum n1 =
		(a.type == NUM_RATIONAL) ? a.value.rational->numerator : a;
	SchemeNum d1 = (a.type == NUM_RATIONAL)
					   ? a.value.rational->denominator
					   : fixnum_create(1);
	SchemeNum n2 =
		(b.type == NUM_RATIONAL) ? b.value.rational->numerator : b;
	SchemeNum d2 = (b.type == NUM_RATIONAL)
					   ? b.value.rational->denominator
					   : fixnum_create(1);

	SchemeNum t1 = schemenum_mul(n1, d2);
	SchemeNum t2 = schemenum_mul(n2, d1);
	SchemeNum top = schemenum_add(t1, t2);

	schemenum_free(t1);
	schemenum_free(t2);

	SchemeNum bot = schemenum_mul(d1, d2);

	return rational_create(top, bot);
}

static SchemeNum sub_rational(SchemeNum a, SchemeNum b)
{
	SchemeNum n1 =
		(a.type == NUM_RATIONAL) ? a.value.rational->numerator : a;
	SchemeNum d1 = (a.type == NUM_RATIONAL)
					   ? a.value.rational->denominator
					   : fixnum_create(1);
	SchemeNum n2 =
		(b.type == NUM_RATIONAL) ? b.value.rational->numerator : b;
	SchemeNum d2 = (b.type == NUM_RATIONAL)
					   ? b.value.rational->denominator
					   : fixnum_create(1);

	SchemeNum t1 = schemenum_mul(n1, d2);
	SchemeNum t2 = schemenum_mul(n2, d1);

	SchemeNum top = schemenum_sub(t1, t2);

	schemenum_free(t1);
	schemenum_free(t2);

	SchemeNum bot = schemenum_mul(d1, d2);

	return rational_create(top, bot);
}

static SchemeNum mul_rational(SchemeNum a, SchemeNum b)
{
	SchemeNum n1 =
		(a.type == NUM_RATIONAL) ? a.value.rational->numerator : a;
	SchemeNum d1 = (a.type == NUM_RATIONAL)
					   ? a.value.rational->denominator
					   : fixnum_create(1);
	SchemeNum n2 =
		(b.type == NUM_RATIONAL) ? b.value.rational->numerator : b;
	SchemeNum d2 = (b.type == NUM_RATIONAL)
					   ? b.value.rational->denominator
					   : fixnum_create(1);

	SchemeNum top = schemenum_mul(n1, n2);
	SchemeNum bot = schemenum_mul(d1, d2);
	return rational_create(top, bot);
}

static SchemeNum div_rational(SchemeNum a, SchemeNum b)
{
	SchemeNum n1 =
		(a.type == NUM_RATIONAL) ? a.value.rational->numerator : a;
	SchemeNum d1 = (a.type == NUM_RATIONAL)
					   ? a.value.rational->denominator
					   : fixnum_create(1);
	SchemeNum n2 =
		(b.type == NUM_RATIONAL) ? b.value.rational->numerator : b;
	SchemeNum d2 = (b.type == NUM_RATIONAL)
					   ? b.value.rational->denominator
					   : fixnum_create(1);

	SchemeNum top = schemenum_mul(n1, d2);
	SchemeNum bot = schemenum_mul(d1, n2);
	return rational_create(top, bot);
}

static SchemeNum mul_complex(SchemeNum a, SchemeNum b)
{
	SchemeNum r1 =
		(a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
	SchemeNum i1 = (a.type == NUM_COMPLEX) ? a.value.complex_num->imag
										   : fixnum_create(0);
	SchemeNum r2 =
		(b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
	SchemeNum i2 = (b.type == NUM_COMPLEX) ? b.value.complex_num->imag
										   : fixnum_create(0);

	// (r1*r2 - i1*i2) + (r1*i2 + i1*r2)i
	SchemeNum t1 = schemenum_mul(r1, r2);
	SchemeNum t2 = schemenum_mul(i1, i2);
	SchemeNum real_part = schemenum_sub(t1, t2);
	schemenum_free(t1);
	schemenum_free(t2);

	SchemeNum t3 = schemenum_mul(r1, i2);
	SchemeNum t4 = schemenum_mul(i1, r2);
	SchemeNum imag_part = schemenum_add(t3, t4);
	schemenum_free(t3);
	schemenum_free(t4);

	return complex_create(real_part, imag_part);
}

static SchemeNum div_complex(SchemeNum a, SchemeNum b)
{
	SchemeNum r1 =
		(a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
	SchemeNum i1 = (a.type == NUM_COMPLEX) ? a.value.complex_num->imag
										   : fixnum_create(0);
	SchemeNum r2 =
		(b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
	SchemeNum i2 = (b.type == NUM_COMPLEX) ? b.value.complex_num->imag
										   : fixnum_create(0);

	// Denominator = r2^2 + i2^2
	SchemeNum t1 = schemenum_mul(r2, r2);
	SchemeNum t2 = schemenum_mul(i2, i2);
	SchemeNum denom = schemenum_add(t1, t2);
	schemenum_free(t1);
	schemenum_free(t2);

	// Numerator Real = r1*r2 + i1*i2
	SchemeNum t3 = schemenum_mul(r1, r2);
	SchemeNum t4 = schemenum_mul(i1, i2);
	SchemeNum top_r = schemenum_add(t3, t4);
	schemenum_free(t3);
	schemenum_free(t4);

	// Numerator Imag = i1*r2 - r1*i2
	SchemeNum t5 = schemenum_mul(i1, r2);
	SchemeNum t6 = schemenum_mul(r1, i2);
	SchemeNum top_i = schemenum_sub(t5, t6);
	schemenum_free(t5);
	schemenum_free(t6);

	SchemeNum real_final = schemenum_div(top_r, denom);
	SchemeNum imag_final = schemenum_div(top_i, denom);

	schemenum_free(top_r);
	schemenum_free(top_i);
	schemenum_free(denom);

	return complex_create(real_final, imag_final);
}

static SchemeNum add_integer(SchemeNum a, SchemeNum b)
{
	if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM)
	{
		int64_t res;
		if (!__builtin_add_overflow(a.value.fixnum, b.value.fixnum,
									&res))
		{
			return fixnum_create(res);
		}
	}

	SchemeNum ba =
		(a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
	SchemeNum bb =
		(b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;
	return bn_add_signed(ba, bb);
}

SchemeNum schemenum_sub(SchemeNum a, SchemeNum b)
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
	SchemeNum r1 =
		(a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
	SchemeNum i1 = (a.type == NUM_COMPLEX) ? a.value.complex_num->imag
										   : fixnum_create(0);
	SchemeNum r2 =
		(b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
	SchemeNum i2 = (b.type == NUM_COMPLEX) ? b.value.complex_num->imag
										   : fixnum_create(0);
	return complex_create(schemenum_sub(r1, r2),
						  schemenum_sub(i1, i2));
}

static SchemeNum sub_integer(SchemeNum a, SchemeNum b)
{
	if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM)
	{
		int64_t res;
		if (!__builtin_sub_overflow(a.value.fixnum, b.value.fixnum,
									&res))
		{
			return fixnum_create(res);
		}
	}
	SchemeNum ba =
		(a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
	SchemeNum bb =
		(b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;
	return bn_sub_signed(ba, bb);
}

SchemeNum schemenum_mul(SchemeNum a, SchemeNum b)
{
	if (a.type == NUM_COMPLEX || b.type == NUM_COMPLEX)
		return mul_complex(a, b);
	if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
		return floatnum_create(to_double(a) * to_double(b));
	if (a.type == NUM_RATIONAL || b.type == NUM_RATIONAL)
		return mul_rational(a, b);
	return mul_integer(a, b);
}

static SchemeNum mul_integer(SchemeNum a, SchemeNum b)
{
	if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM)
	{
		int64_t res;
		if (!__builtin_mul_overflow(a.value.fixnum, b.value.fixnum,
									&res))
		{
			return fixnum_create(res);
		}
	}
	SchemeNum ba =
		(a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
	SchemeNum bb =
		(b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;
	return bn_mul_signed(ba, bb);
}

SchemeNum schemenum_div(SchemeNum a, SchemeNum b)
{
	if (a.type == NUM_COMPLEX || b.type == NUM_COMPLEX)
		return div_complex(a, b);
	if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
	{
		double db = to_double(b);
		if (db == 0.0)
		{
			fprintf(stderr, "Error: Division by zero (float)\n");
			return floatnum_create(0.0);
		}
		return floatnum_create(to_double(a) / db);
	}
	if (a.type == NUM_RATIONAL || b.type == NUM_RATIONAL)
		return div_rational(a, b);

	return div_integer(a, b);
}

bool schemenum_eq(SchemeNum a, SchemeNum b)
{
	// Complex equality
	if (a.type == NUM_COMPLEX || b.type == NUM_COMPLEX)
	{
		SchemeNum r1 =
			(a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
		SchemeNum i1 = (a.type == NUM_COMPLEX)
						   ? a.value.complex_num->imag
						   : fixnum_create(0);
		SchemeNum r2 =
			(b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
		SchemeNum i2 = (b.type == NUM_COMPLEX)
						   ? b.value.complex_num->imag
						   : fixnum_create(0);
		return schemenum_eq(r1, r2) && schemenum_eq(i1, i2);
	}

	// Float (Inexact) contagion
	if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
	{
		return to_double(a) == to_double(b);
	}

	// Rational vs (Rational or Integer)
	if (a.type == NUM_RATIONAL || b.type == NUM_RATIONAL)
	{
		SchemeNum n1 = (a.type == NUM_RATIONAL)
						   ? a.value.rational->numerator
						   : a;
		SchemeNum d1 = (a.type == NUM_RATIONAL)
						   ? a.value.rational->denominator
						   : fixnum_create(1);
		SchemeNum n2 = (b.type == NUM_RATIONAL)
						   ? b.value.rational->numerator
						   : b;
		SchemeNum d2 = (b.type == NUM_RATIONAL)
						   ? b.value.rational->denominator
						   : fixnum_create(1);

		SchemeNum left = schemenum_mul(n1, d2);
		SchemeNum right = schemenum_mul(n2, d1);
		bool res = schemenum_eq(left, right);
		schemenum_free(left);
		schemenum_free(right);
		return res;
	}

	// Integer (Bignum/Fixnum) comparison
	if (a.type == NUM_BIGNUM || b.type == NUM_BIGNUM)
	{
		SchemeNum ba =
			(a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
		SchemeNum bb =
			(b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;
		bool res =
			(bn_cmp_mag(ba.value.bignum, bb.value.bignum) == 0 &&
			 ba.value.bignum->sign == bb.value.bignum->sign);
		if (a.type == NUM_FIXNUM)
			schemenum_free(ba);
		if (b.type == NUM_FIXNUM)
			schemenum_free(bb);
		return res;
	}

	return a.value.fixnum == b.value.fixnum;
}

bool schemenum_lt(SchemeNum a, SchemeNum b)
{
	if (!schemenum_is_real(a) || !schemenum_is_real(b))
		return false;

	if (a.type == NUM_COMPLEX || b.type == NUM_COMPLEX)
	{
		SchemeNum r1 =
			(a.type == NUM_COMPLEX) ? a.value.complex_num->real : a;
		SchemeNum r2 =
			(b.type == NUM_COMPLEX) ? b.value.complex_num->real : b;
		return schemenum_lt(r1, r2);
	}

	if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
	{
		return to_double(a) < to_double(b);
	}

	if (a.type == NUM_RATIONAL || b.type == NUM_RATIONAL)
	{
		SchemeNum n1 = (a.type == NUM_RATIONAL)
						   ? a.value.rational->numerator
						   : a;
		SchemeNum d1 = (a.type == NUM_RATIONAL)
						   ? a.value.rational->denominator
						   : fixnum_create(1);
		SchemeNum n2 = (b.type == NUM_RATIONAL)
						   ? b.value.rational->numerator
						   : b;
		SchemeNum d2 = (b.type == NUM_RATIONAL)
						   ? b.value.rational->denominator
						   : fixnum_create(1);

		SchemeNum left = schemenum_mul(n1, d2);
		SchemeNum right = schemenum_mul(n2, d1);
		bool res = schemenum_lt(left, right);
		schemenum_free(left);
		schemenum_free(right);
		return res;
	}

	if (a.type == NUM_BIGNUM || b.type == NUM_BIGNUM)
	{
		SchemeNum ba =
			(a.type == NUM_FIXNUM) ? bn_from_fix(a.value.fixnum) : a;
		SchemeNum bb =
			(b.type == NUM_FIXNUM) ? bn_from_fix(b.value.fixnum) : b;
		int cmp;
		if (ba.value.bignum->sign != bb.value.bignum->sign)
		{
			cmp = (ba.value.bignum->sign < bb.value.bignum->sign) ? -1
																  : 1;
		}
		else
		{
			cmp = bn_cmp_mag(ba.value.bignum, bb.value.bignum);
			if (ba.value.bignum->sign == -1)
				cmp = -cmp;
		}
		if (a.type == NUM_FIXNUM)
			schemenum_free(ba);
		if (b.type == NUM_FIXNUM)
			schemenum_free(bb);
		return cmp < 0;
	}

	return a.value.fixnum < b.value.fixnum;
}

bool schemenum_gt(SchemeNum a, SchemeNum b)
{
	if (!schemenum_is_real(a) || !schemenum_is_real(b))
		return false;
	return schemenum_lt(b, a);
}

bool schemenum_is_integer(SchemeNum a)
{
	if (a.type == NUM_FIXNUM || a.type == NUM_BIGNUM)
		return true;
	if (a.type == NUM_FLOAT)
	{
		return a.value.floatnum == floor(a.value.floatnum);
	}
	if (a.type == NUM_RATIONAL)
	{
		return (a.value.rational->denominator.value.fixnum == 1);
	}
	if (a.type == NUM_COMPLEX)
	{
		return schemenum_is_integer(a.value.complex_num->real) &&
			   to_double(a.value.complex_num->imag) == 0;
	}
	return false;
}

static bool is_effectively_integer(SchemeNum n)
{
	if (n.type == NUM_FIXNUM || n.type == NUM_BIGNUM)
		return true;
	if (n.type == NUM_FLOAT)
		return n.value.floatnum == trunc(n.value.floatnum);
	if (n.type == NUM_RATIONAL)
		return (n.value.rational->denominator.value.fixnum == 1);
	return false;
}

bool schemenum_is_zero(SchemeNum n)
{
	switch (n.type)
	{
	case NUM_FIXNUM:
		return n.value.fixnum == 0;
	case NUM_FLOAT:
		return n.value.floatnum == 0.0;
	case NUM_BIGNUM:
		return bn_is_zero(n.value.bignum);
	case NUM_RATIONAL:
		return schemenum_is_zero(n.value.rational->numerator);
	case NUM_COMPLEX:
		return schemenum_is_zero(n.value.complex_num->real) &&
			   schemenum_is_zero(n.value.complex_num->imag);
	default:
		return false;
	}
}

SchemeNum schemenum_quotient(SchemeNum a, SchemeNum b)
{
	if (to_double(b) == 0.0)
		return fixnum_create(0);

	if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM)
	{
		return fixnum_create(a.value.fixnum / b.value.fixnum);
	}

	if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
	{
		return floatnum_create(trunc(to_double(a) / to_double(b)));
	}

	if (is_effectively_integer(a) && is_effectively_integer(b))
	{
		SchemeNum q, r;
		bn_div_rem(a, b, &q, &r);
		schemenum_free(r);
		return q;
	}

	return floatnum_create(trunc(to_double(a) / to_double(b)));
}

SchemeNum schemenum_remainder(SchemeNum a, SchemeNum b)
{
	if (to_double(b) == 0.0)
		return fixnum_create(0);

	if (a.type == NUM_FIXNUM && b.type == NUM_FIXNUM)
	{
		return fixnum_create(a.value.fixnum % b.value.fixnum);
	}

	if (a.type == NUM_FLOAT || b.type == NUM_FLOAT)
	{
		return floatnum_create(fmod(to_double(a), to_double(b)));
	}

	if (is_effectively_integer(a) && is_effectively_integer(b))
	{
		SchemeNum q, r;
		bn_div_rem(a, b, &q, &r);
		schemenum_free(q);
		return r;
	}

	return floatnum_create(fmod(to_double(a), to_double(b)));
}

SchemeNum schemenum_modulo(SchemeNum a, SchemeNum b)
{
	SchemeNum rem = schemenum_remainder(a, b);
	if (!schemenum_is_zero(rem))
	{
		double r_val = to_double(rem);
		double b_val = to_double(b);
		if ((r_val < 0) != (b_val < 0))
		{
			return schemenum_add(rem, b);
		}
	}
	return rem;
}

bool schemenum_is_exact(SchemeNum a) { return a.exact; }

SchemeNum schemenum_to_exact(SchemeNum a)
{
	if (a.exact)
		return a;
	if (a.type == NUM_FLOAT)
	{
		return bignum_create_from_fix((int64_t)a.value.floatnum);
	}
	a.exact = true;
	return a;
}

SchemeNum schemenum_to_inexact(SchemeNum a)
{
	if (!a.exact)
		return a;
	return floatnum_create(to_double(a));
}

void schemenum_free(SchemeNum num)
{
	switch (num.type)
	{
	case NUM_FIXNUM:
	case NUM_FLOAT:
		break;
	case NUM_BIGNUM:
		if (num.value.bignum)
		{
			free(num.value.bignum->limbs);
			free(num.value.bignum);
		}
		break;
	case NUM_RATIONAL:
		if (num.value.rational)
		{
			schemenum_free(num.value.rational->numerator);
			schemenum_free(num.value.rational->denominator);
			free(num.value.rational);
		}
		break;
	case NUM_COMPLEX:
		if (num.value.complex_num)
		{
			schemenum_free(num.value.complex_num->real);
			schemenum_free(num.value.complex_num->imag);
			free(num.value.complex_num);
		}
		break;
	}
}

static bool bn_is_zero(BigNum *bn)
{
	return bn->len == 0 || (bn->len == 1 && bn->limbs[0] == 0);
}

void schemenum_print(SchemeNum num)
{
	switch (num.type)
	{
	case NUM_FIXNUM:
		printf("%" PRId64, num.value.fixnum);
		break;

	case NUM_FLOAT:
		if (num.value.floatnum == floor(num.value.floatnum))
		{
			printf("%.1f", num.value.floatnum);
		}
		else
		{
			printf("%g", num.value.floatnum);
		}
		break;

	case NUM_BIGNUM:
	{
		if (bn_is_zero(num.value.bignum))
		{
			printf("0");
			return;
		}
		if (num.value.bignum->sign == -1)
			printf("-");

		SchemeNum mag = bignum_alloc(num.value.bignum->len);
		memcpy(mag.value.bignum->limbs, num.value.bignum->limbs,
			   num.value.bignum->len * sizeof(uint32_t));
		mag.value.bignum->len = num.value.bignum->len;
		mag.value.bignum->sign = 1;

		uint32_t chunks[64];
		int count = 0;
		SchemeNum divisor = fixnum_create(1000000000);

		while (!bn_is_zero(mag.value.bignum))
		{
			SchemeNum q, r;
			bn_div_rem(mag, divisor, &q, &r);
			chunks[count++] = (uint32_t)r.value.fixnum;

			schemenum_free(mag);
			mag = q;
			schemenum_free(r);
		}

		printf("%u", chunks[--count]);
		while (count > 0)
		{
			printf("%09u", chunks[--count]);
		}
		schemenum_free(mag);
		break;
	}

	case NUM_RATIONAL:
		schemenum_print(num.value.rational->numerator);
		printf("/");
		schemenum_print(num.value.rational->denominator);
		break;

	case NUM_COMPLEX:
	{
		SchemeNum real = num.value.complex_num->real;
		SchemeNum imag = num.value.complex_num->imag;

		bool real_zero = (to_double(real) == 0.0);
		if (!real_zero)
		{
			schemenum_print(real);
		}

		double im_val = to_double(imag);
		if (im_val >= 0 && !real_zero)
			printf("+");

		if (im_val == 1.0 && imag.exact)
		{
		}
		else if (im_val == -1.0 && imag.exact)
		{
			printf("-");
		}
		else
		{
			schemenum_print(imag);
		}
		printf("i");
		break;
	}
	}
}