#pragma once
#include "value.h"

Value* intern(const char* s);
void intern_init(void);
void intern_cleanup(void);
