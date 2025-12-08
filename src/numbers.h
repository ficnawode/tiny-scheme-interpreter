#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef double FloatNum;
typedef int64_t FixNum;

struct BigNum;
struct Rational;
struct ComplexNum;

typedef enum {
    NUM_FIXNUM,
    NUM_BIGNUM,
    NUM_RATIONAL,
    NUM_FLOAT,
    NUM_COMPLEX
} SchemeNumType;

typedef struct BigNum {
    int sign;
    size_t len;
    size_t capacity;
    uint32_t* limbs;
} BigNum;

typedef struct SchemeNum {
    SchemeNumType type;
    bool exact;

    union {
        FixNum fixnum;
        FloatNum floatnum;

        struct BigNum* bignum;
        struct Rational* rational;
        struct ComplexNum* complex_num;
    } value;
} SchemeNum;

typedef struct Rational {
    SchemeNum numerator;
    SchemeNum denominator;
} Rational;

typedef struct ComplexNum {
    SchemeNum real;
    SchemeNum imag;
} ComplexNum;

SchemeNum fixnum_create(int64_t val);
SchemeNum floatnum_create(double val);
SchemeNum bignum_create_from_fix(int64_t val);
SchemeNum rational_create(SchemeNum num, SchemeNum den);
SchemeNum complex_create(SchemeNum real, SchemeNum imag);

SchemeNum scheme_add(SchemeNum a, SchemeNum b);
SchemeNum scheme_sub(SchemeNum a, SchemeNum b);
SchemeNum scheme_mul(SchemeNum a, SchemeNum b);
SchemeNum scheme_div(SchemeNum a, SchemeNum b);
