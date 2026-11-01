.PHONY: all clean backend frontend test test-unit test-e2e test-backend fmt install-hooks

all: backend frontend

fmt:
	@command -v dune >/dev/null || { echo "dune is required for make fmt" >&2; exit 1; }
	@command -v clang-format >/dev/null || { echo "clang-format is required for make fmt" >&2; exit 1; }
	cd frontend && dune fmt
	cd backend && find . -type f \( -name "*.cpp" -o -name "*.h" -o -name "*.hpp" -o -name "*.cc" \) -exec clang-format -i -style=file {} +

install-hooks:
	git config --local core.hooksPath .githooks

backend: 
	cmake -S backend -B backend/build -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
	cmake --build backend/build --parallel

frontend: 
	cd frontend && dune build ./src/blink.exe && cp ./_build/default/src/blink.exe ../blink -f

test:
	cd frontend && dune runtest

test-unit:
	cd frontend && dune exec ./test/frontend_tests.exe

test-e2e:
	cd frontend && dune exec ./test/e2e.exe

test-backend:
	cd frontend && dune build ./test/backend_fixture_compiler.exe && dune exec ./test/backend_tests.exe

clean: 
	@rm -rf backend/build *.{s,o,ll} && cd frontend && dune clean
