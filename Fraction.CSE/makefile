# Variables
CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++11
SRCS = fraction_basic.cpp fraction_comparison.cpp fraction_math.cpp
OBJS = ${SRCS:.cpp=.o}
PROGS = ${SRCS:.cpp=}
DEPS = fraction.h test.h
DEPOBJS = fraction.o

# Default rule. Make all programs.
all: $(PROGS)
.PHONY: all

# Make an individual program.
$(PROGS): % : %.o $(DEPOBJS)
	$(CXX) $(CXXFLAGS) $(DEPOBJS) $@.o -o $@

# Make an object file.
%.o: %.cpp $(DEPS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean Up.
.PHONY: clean
clean:
	@echo "Cleaning up"
	@rm -f $(PROGS) $(OBJS) $(DEPOBJS)

