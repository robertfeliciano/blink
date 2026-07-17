.PHONY: all clean backend frontend

all: backend frontend

backend: 
	cmake -S backend -B backend/build -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
	cmake --build backend/build --parallel

frontend: 
	cd frontend && dune build && mv ./_build/default/src/blink.exe ../blink -f

clean: 
	@rm -rf backend/build *.{s,o,ll} && cd frontend && dune clean
