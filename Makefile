CC := gcc
CFLAGS := -Wall -Wextra -g
LDFLAGS := -lreadline

SRCDIR := src
OBJDIR := obj
BINDIR := bin

EXECUTABLE := $(BINDIR)/scheme

SOURCES := $(wildcard $(SRCDIR)/*.c)

OBJECTS := $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SOURCES))

all: $(EXECUTABLE)

test: $(EXECUTABLE) ${test}
	${EXECUTABLE} test/main.scm 

$(EXECUTABLE): $(OBJECTS)
	@mkdir -p $(@D)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	@rm -rf $(OBJDIR) $(BINDIR)

.PHONY: all clean
