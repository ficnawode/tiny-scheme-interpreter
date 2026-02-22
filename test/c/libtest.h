
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef void (*TestFunc)(void);

typedef struct TestCase
{
	const char *suite;
	const char *name;
	TestFunc func;
	struct TestCase *next;
} TestCase;

TestCase *g_head = NULL;

int g_tests_run = 0;
int g_tests_passed = 0;
int g_tests_failed = 0;

void register_test(const char *suite, const char *name, TestFunc func)
{
	TestCase *tc = (TestCase *)malloc(sizeof(TestCase));
	tc->suite = suite;
	tc->name = name;
	tc->func = func;
	tc->next = g_head;
	g_head = tc;
}

#define TEST(suite, name)                                            \
	void test_##suite##_##name(void);                                \
	__attribute__((constructor)) void register_##suite##_##name(     \
		void)                                                        \
	{                                                                \
		register_test(#suite, #name, test_##suite##_##name);         \
	}                                                                \
	void test_##suite##_##name(void)

#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_RESET "\x1b[0m"

void fail_test(const char *file, int line, const char *fmt, ...)
{
	printf(ANSI_COLOR_RED "    FAILURE %s:%d: " ANSI_COLOR_RESET,
		   file, line);
	va_list args;
	va_start(args, fmt);
	vprintf(fmt, args);
	printf("\n");
	va_end(args);
	g_tests_failed++;
}

#define ASSERT_TRUE(condition)                                       \
	do                                                               \
	{                                                                \
		if (!(condition))                                            \
		{                                                            \
			fail_test(__FILE__, __LINE__,                            \
					  "Expected true: " #condition);                 \
			return;                                                  \
		}                                                            \
	} while (0)

#define ASSERT_FALSE(condition)                                      \
	do                                                               \
	{                                                                \
		if (condition)                                               \
		{                                                            \
			fail_test(__FILE__, __LINE__,                            \
					  "Expected false: " #condition);                 \
			return;                                                  \
		}                                                            \
	} while (0)

#define ASSERT_EQ_INT(expected, actual)                              \
	do                                                               \
	{                                                                \
		int64_t e = (expected);                                      \
		int64_t a = (actual);                                        \
		if (e != a)                                                  \
		{                                                            \
			fail_test(__FILE__, __LINE__,                            \
					  "Expected: %ld, Actual: %ld", e, a);           \
			return;                                                  \
		}                                                            \
	} while (0)

#define ASSERT_EQ_DOUBLE(expected, actual)                           \
	do                                                               \
	{                                                                \
		double e = (expected);                                       \
		double a = (actual);                                         \
		if (fabs(e - a) > 0.000001)                                  \
		{                                                            \
			fail_test(__FILE__, __LINE__,                            \
					  "Expected: %f, Actual: %f", e, a);             \
			return;                                                  \
		}                                                            \
	} while (0)

#define ASSERT_TYPE(num, expected_type)                              \
	do                                                               \
	{                                                                \
		if ((num).type != (expected_type))                           \
		{                                                            \
			fail_test(__FILE__, __LINE__,                            \
					  "Wrong type. Expected %d, got %d",             \
					  expected_type, (num).type);                    \
			return;                                                  \
		}                                                            \
	} while (0)

int RUN_ALL_TESTS()
{
	printf("==================================================\n");

	TestCase *current = g_head;

	while (current)
	{
		printf("[ RUN      ] %s.%s\n", current->suite, current->name);

		int failures_before = g_tests_failed;
		current->func();

		if (g_tests_failed == failures_before)
		{
			printf(ANSI_COLOR_GREEN "[       OK ] " ANSI_COLOR_RESET
									"%s.%s\n",
				   current->suite, current->name);
			g_tests_passed++;
		}
		else
		{
			printf(ANSI_COLOR_RED "[  FAILED  ] " ANSI_COLOR_RESET
								  "%s.%s\n",
				   current->suite, current->name);
		}

		g_tests_run++;
		current = current->next;
	}

	printf("==================================================\n");
	printf("SUMMARY: %d tests, %d passed, ", g_tests_run,
		   g_tests_passed);
	if (g_tests_failed > 0)
	{
		printf(ANSI_COLOR_RED "%d failed" ANSI_COLOR_RESET "\n",
			   g_tests_failed);
		return 1;
	}
	else
	{
		printf(ANSI_COLOR_GREEN "0 failed" ANSI_COLOR_RESET "\n");
		return 0;
	}
}