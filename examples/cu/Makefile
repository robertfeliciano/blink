ROOT := $(abspath ../..)
NVCC ?= nvcc
LLC ?= llc
CLANG ?= clang
OPT ?= -O0

.PHONY: all run clean
all: build/vector-add

build:
	mkdir -p $@

build/vector_add.o: $(ROOT)/runtime/stdlib/cu/vector_add.cu | build
	$(NVCC) -std=c++14 -c $< -o $@

# Always regenerate IR so changes to imported Blink modules are included.
.PHONY: build/new_output.s
build/new_output.s: main.bl | build
	cd build && "$(ROOT)/blink" $(OPT) -stdlib-root "$(ROOT)/runtime/stdlib" "$(CURDIR)/main.bl"
	$(LLC) --relocation-model=pic build/new_output.ll -o $@

build/main.o: build/new_output.s
	$(CLANG) -c $< -o $@

build/vector-add: build/main.o build/vector_add.o
	$(NVCC) $^ -o $@ -lm

run: all
	./build/vector-add

clean:
	rm -rf build
