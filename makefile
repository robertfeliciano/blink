.PHONY: all clean backend frontend test test-unit test-e2e test-backend

all: backend frontend

backend: 
	cmake -S backend -B backend/build -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
	cmake --build backend/build --parallel

frontend: 
	cd frontend && dune build && mv ./_build/default/src/blink.exe ../blink -f

test:
	cd frontend && dune runtest

test-unit:
	cd frontend && dune exec ./test/frontend_tests.exe

test-e2e:
	cd frontend && dune exec ./test/e2e.exe

test-backend:
	cd frontend && dune exec ./test/backend_tests.exe

clean: 
	@rm -rf backend/build *.{s,o,ll} && cd frontend && dune clean
