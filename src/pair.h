#pragma once

#include "value.h"

#define NIL value_get_nil()

#define CAR(x) ((x)->u.pair.car)
#define CDR(x) ((x)->u.pair.cdr)

#define CADR(x) CAR(CDR(x))
#define CADDR(x) CAR(CDR(CDR(x)))
#define CADDDR(x) CAR(CDR(CDR(CDR(x))))

#define CONS(a, d) value_cons_create((a), (d))
