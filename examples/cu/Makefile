ROOT := $(abspath ../..)
NVCC ?= nvcc
LLC ?= llc
CLANG ?= clang
OPT ?= -O0

.PHONY: all run test clean
all: build/vector-add build/gemm

build:
	mkdir -p $@

build/vector_add.o: $(ROOT)/runtime/stdlib/cu/vector_add.cu | build
	$(NVCC) -std=c++14 -c $< -o $@

build/gemm.o: $(ROOT)/runtime/stdlib/cu/gemm.cu $(ROOT)/runtime/stdlib/cu/gemm.h | build
	$(NVCC) -std=c++14 -c $< -o $@

# Rebuild imported modules and isolate each compiler invocation's fixed IR name.
.PHONY: FORCE
FORCE:

build/ir/%/main.o: %.bl FORCE | build
	mkdir -p "$(@D)"
	cd "$(@D)" && "$(ROOT)/blink" $(OPT) -stdlib-root "$(ROOT)/runtime/stdlib" "$(CURDIR)/$<"
	$(LLC) --relocation-model=pic "$(@D)/new_output.ll" -o "$(@D)/main.s"
	$(CLANG) -c "$(@D)/main.s" -o $@

build/vector-add: build/ir/main/main.o build/vector_add.o
	$(NVCC) $^ -o $@ -lm

build/gemm: build/ir/gemm/main.o build/gemm.o
	$(NVCC) $^ -o $@ -lcublas -lm

build/gemm-test: gemm_test.cpp build/gemm.o
	$(NVCC) -std=c++14 $^ -o $@ -lcublas

test: build/gemm-test
	./build/gemm-test

run: all
	./build/vector-add
	./build/gemm

clean:
	rm -rf build
