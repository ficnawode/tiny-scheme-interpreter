CC := gcc

ifndef NO_READLINE
  CFLAGS += -DUSE_READLINE
  LDFLAGS += -lreadline
  USE_READLINE := yes
else
  USE_READLINE := no
endif

SRCDIR := src
OBJDIR := obj
BINDIR := bin
TESTDIR := test/c

CFLAGS := -Wall -Wextra -g -I$(SRCDIR)  
LDFLAGS := -lm                          

EXECUTABLE := $(BINDIR)/scheme
TEST_EXECUTABLE := $(BINDIR)/test_runner

SOURCES := $(wildcard $(SRCDIR)/*.c)
OBJECTS := $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SOURCES))

APP_OBJECTS := $(filter-out $(OBJDIR)/main.o, $(OBJECTS))

TEST_SOURCES := $(wildcard $(TESTDIR)/*.c)
TEST_OBJECTS := $(patsubst $(TESTDIR)/%.c,$(OBJDIR)/%.o,$(TEST_SOURCES))

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	@mkdir -p $(@D)
	@echo "Linking with readline: $(USE_READLINE)"
	@if $(CC) $(OBJECTS) -o $@ $(LDFLAGS); then \
	    echo "Build successful."; \
	else \
	    echo "Warning: Failed to link with readline."; \
	    echo "If you do not have GNU readline, try: make NO_READLINE=1"; \
	    exit 1; \
	fi

$(OBJDIR)/%.o: $(TESTDIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

$(TEST_EXECUTABLE): $(APP_OBJECTS) $(TEST_OBJECTS)
	@mkdir -p $(@D)
	$(CC) $(APP_OBJECTS) $(TEST_OBJECTS) -o $@ $(LDFLAGS)

test-c: $(TEST_EXECUTABLE)
	@echo "Running C Unit Tests..."
	@$(TEST_EXECUTABLE)

test-scm: $(EXECUTABLE)
	${EXECUTABLE} test/main.scm

test: test-c test-scm

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	@rm -rf $(OBJDIR) $(BINDIR)

.PHONY: all clean test test-c test-scm
