# Makefile for SUEWS C implementation
# Proof-of-concept for Fortran-to-C port

CC = gcc
CFLAGS = -Wall -Wextra -O2 -std=c99 -pedantic
LDFLAGS = -lm

# Source files
SOURCES = suews_phys_evap.c
HEADERS = suews_phys_evap.h
TEST_SRC = test_evap.c

# Object files
OBJECTS = $(SOURCES:.c=.o)
TEST_OBJ = $(TEST_SRC:.c=.o)

# Targets
TARGET = test_evap
LIBRARY = libsuews_evap.a

.PHONY: all clean test

all: $(TARGET)

# Build test executable
$(TARGET): $(OBJECTS) $(TEST_OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Build static library
$(LIBRARY): $(OBJECTS)
	ar rcs $@ $^

# Compile object files
%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

# Run tests
test: $(TARGET)
	./$(TARGET)

# Clean build artifacts
clean:
	rm -f $(OBJECTS) $(TEST_OBJ) $(TARGET) $(LIBRARY)

# Show compiler and flags
info:
	@echo "Compiler: $(CC)"
	@echo "Flags: $(CFLAGS)"
	@echo "Sources: $(SOURCES)"
	@echo "Headers: $(HEADERS)"
