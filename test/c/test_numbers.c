#include "numbers.h"
#include "libtest.h"

TEST(Arithmetic, IntegerDivExact)
{
	SchemeNum a = fixnum_create(10);
	SchemeNum b = fixnum_create(2);
	SchemeNum res = schemenum_div(a, b);
	ASSERT_TYPE(res, NUM_FIXNUM);
	ASSERT_EQ_INT(5, res.value.fixnum);
}

TEST(Arithmetic, IntegerDivToRational)
{
	SchemeNum a = fixnum_create(1);
	SchemeNum b = fixnum_create(2);
	SchemeNum res = schemenum_div(a, b);
	ASSERT_TYPE(res, NUM_RATIONAL);
	ASSERT_EQ_INT(1, res.value.rational->numerator.value.fixnum);
	ASSERT_EQ_INT(2, res.value.rational->denominator.value.fixnum);
}

TEST(Arithmetic, RationalSimplificationOnSub)
{
	SchemeNum r = rational_create(fixnum_create(1), fixnum_create(2));
	SchemeNum res = schemenum_sub(r, r);
	ASSERT_TYPE(res, NUM_FIXNUM);
	ASSERT_EQ_INT(0, res.value.fixnum);
}

TEST(Arithmetic, FloatDiv)
{
	SchemeNum a = floatnum_create(5.0);
	SchemeNum b = floatnum_create(2.0);
	SchemeNum res = schemenum_div(a, b);
	ASSERT_TYPE(res, NUM_FLOAT);
	ASSERT_EQ_DOUBLE(2.5, res.value.floatnum);
}

TEST(Arithmetic, MixedMulContagion)
{
	SchemeNum a = fixnum_create(2);
	SchemeNum b = floatnum_create(2.5);
	SchemeNum res = schemenum_mul(a, b);
	ASSERT_TYPE(res, NUM_FLOAT);
	ASSERT_EQ_DOUBLE(5.0, res.value.floatnum);
}

TEST(Arithmetic, ComplexDiv)
{
	SchemeNum c1 = complex_create(fixnum_create(1), fixnum_create(1));
	SchemeNum c2 =
		complex_create(fixnum_create(1), fixnum_create(-1));
	SchemeNum res = schemenum_div(c1, c2);

	ASSERT_TYPE(res, NUM_COMPLEX);

	ASSERT_TYPE(res.value.complex_num->real, NUM_FIXNUM);
	ASSERT_EQ_INT(0, res.value.complex_num->real.value.fixnum);

	ASSERT_TYPE(res.value.complex_num->imag, NUM_FIXNUM);
	ASSERT_EQ_INT(1, res.value.complex_num->imag.value.fixnum);
}

TEST(Comparison, EqInteger)
{
	ASSERT_TRUE(schemenum_eq(fixnum_create(5), fixnum_create(5)));
	ASSERT_FALSE(schemenum_eq(fixnum_create(5), fixnum_create(6)));
}

TEST(Comparison, EqFloat)
{
	ASSERT_TRUE(
		schemenum_eq(floatnum_create(3.14), floatnum_create(3.14)));
	ASSERT_FALSE(
		schemenum_eq(floatnum_create(3.14), floatnum_create(3.15)));
}

TEST(Comparison, EqMixed)
{
	ASSERT_TRUE(schemenum_eq(fixnum_create(5), floatnum_create(5.0)));
	ASSERT_FALSE(
		schemenum_eq(fixnum_create(5), floatnum_create(5.1)));
}

TEST(Comparison, EqRational)
{
	SchemeNum r1 =
		rational_create(fixnum_create(1), fixnum_create(2));
	SchemeNum r2 = rational_create(
		fixnum_create(2), fixnum_create(4));
	ASSERT_TRUE(schemenum_eq(r1, r2));
}

TEST(Comparison, LtInteger)
{
	ASSERT_TRUE(schemenum_lt(fixnum_create(1), fixnum_create(2)));
	ASSERT_FALSE(schemenum_lt(fixnum_create(2), fixnum_create(1)));
}

TEST(Comparison, LtMixed)
{
	ASSERT_TRUE(schemenum_lt(fixnum_create(1), floatnum_create(1.5)));
	ASSERT_FALSE(
		schemenum_lt(floatnum_create(2.0), fixnum_create(1)));
}

TEST(Comparison, GtRational)
{
	SchemeNum r1 =
		rational_create(fixnum_create(1), fixnum_create(2));
	SchemeNum r2 =
		rational_create(fixnum_create(1), fixnum_create(3));
	ASSERT_TRUE(schemenum_gt(r1, r2));
	ASSERT_FALSE(schemenum_gt(r2, r1));
}

TEST(Comparison, ComplexOrdering)
{
	SchemeNum c1 = complex_create(fixnum_create(1), fixnum_create(1));
	SchemeNum c2 = complex_create(fixnum_create(2), fixnum_create(2));
	ASSERT_FALSE(schemenum_lt(c1, c2));
	ASSERT_FALSE(schemenum_gt(c1, c2));
}

TEST(Arithmetic, BigNumDivExact)
{
	int64_t big_val = 10000000000LL;
	
	SchemeNum bn_a = bignum_create_from_fix(big_val);
	SchemeNum bn_b = bignum_create_from_fix(2);
	
	SchemeNum res = schemenum_div(bn_a, bn_b);
	
	ASSERT_TYPE(res, NUM_FIXNUM);
	ASSERT_EQ_INT(5000000000LL, res.value.fixnum);
}

TEST(Arithmetic, BigNumDivRemainder)
{
	SchemeNum a = bignum_create_from_fix(10);
	SchemeNum b = bignum_create_from_fix(3);
	
	SchemeNum res = schemenum_div(a, b);
	
	ASSERT_TYPE(res, NUM_RATIONAL);
	
	ASSERT_TRUE(schemenum_eq(res.value.rational->numerator, a));
	ASSERT_TRUE(schemenum_eq(res.value.rational->denominator, b));
}

TEST(Arithmetic, BigNumDivLargeExact)
{
	int64_t huge = 4611686018427387904LL; 
	SchemeNum a = bignum_create_from_fix(huge);
	SchemeNum a2 = schemenum_add(a, a); 
	
	SchemeNum two = fixnum_create(2);
	SchemeNum res = schemenum_div(a2, two);
	
	ASSERT_TRUE(schemenum_eq(res, a));
}
int main() { return RUN_ALL_TESTS(); }
