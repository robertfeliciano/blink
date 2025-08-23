.PHONY: all clean backend frontend

all: backend frontend

backend: 
	@if [ ! -d backend/build ]; then \
		cd backend && ./builder; \
	fi
	cd backend/build && make

frontend: 
	cd frontend && dune build && mv ./_build/default/src/blink.exe ../blink -f

clean: 
	@rm -rf backend/build && cd frontend && dune clean
