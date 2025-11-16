CC := gcc
CFLAGS := -Wall -Wextra -g
LDFLAGS :=

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

EXECUTABLE := $(BINDIR)/scheme

SOURCES := $(wildcard $(SRCDIR)/*.c)
OBJECTS := $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SOURCES))

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

test: clean $(EXECUTABLE)
	${EXECUTABLE} test/main.scm

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	@rm -rf $(OBJDIR) $(BINDIR)

.PHONY: all clean test
