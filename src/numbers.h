#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef double FloatNum;
typedef int64_t FixNum;

struct BigNum;
struct Rational;
struct ComplexNum;

typedef enum
{
	NUM_FIXNUM,
	NUM_BIGNUM,
	NUM_RATIONAL,
	NUM_FLOAT,
	NUM_COMPLEX
} SchemeNumType;

typedef struct BigNum
{
	int sign;
	size_t len;
	size_t capacity;
	uint32_t *limbs;
} BigNum;

typedef struct SchemeNum
{
	SchemeNumType type;
	bool exact;

	union
	{
		FixNum fixnum;
		FloatNum floatnum;

		struct BigNum *bignum;
		struct Rational *rational;
		struct ComplexNum *complex_num;
	} value;
} SchemeNum;

typedef struct Rational
{
	SchemeNum numerator;
	SchemeNum denominator;
} Rational;

typedef struct ComplexNum
{
	SchemeNum real;
	SchemeNum imag;
} ComplexNum;

SchemeNum fixnum_create(int64_t val);
SchemeNum floatnum_create(double val);
SchemeNum bignum_create_from_fix(int64_t val);
SchemeNum rational_create(SchemeNum num, SchemeNum den);
SchemeNum complex_create(SchemeNum real, SchemeNum imag);

SchemeNum schemenum_add(SchemeNum a, SchemeNum b);
SchemeNum schemenum_sub(SchemeNum a, SchemeNum b);
SchemeNum schemenum_mul(SchemeNum a, SchemeNum b);
SchemeNum schemenum_div(SchemeNum a, SchemeNum b);
bool schemenum_eq(SchemeNum a, SchemeNum b);
bool schemenum_lt(SchemeNum a, SchemeNum b);
bool schemenum_gt(SchemeNum a, SchemeNum b);

SchemeNum schemenum_quotient(SchemeNum a, SchemeNum b);
SchemeNum schemenum_remainder(SchemeNum a, SchemeNum b);
SchemeNum schemenum_modulo(SchemeNum a, SchemeNum b);

bool schemenum_is_exact(SchemeNum a);
SchemeNum schemenum_to_exact(SchemeNum a);
SchemeNum schemenum_to_inexact(SchemeNum a);
bool schemenum_is_real(SchemeNum a);

bool schemenum_is_zero(SchemeNum a);
bool schemenum_is_integer(SchemeNum a);

void schemenum_free(SchemeNum num);
void schemenum_print(SchemeNum num);

double to_double(SchemeNum n);