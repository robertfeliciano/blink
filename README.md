# Blink
And you'll miss it...

### How to build this project

One time I made the mistake of not touching this project for 2 months and I completely forgot how to compile it. 

Run `make` in your terminal.

Alternative, starting in the root directory, `blink/`, run the following commands: 

`cd backend`

`./builder`

`cd build`

`make`

`cd ../../frontend`

`dune build`

`dune exec src/blink.exe -- <filename>`

I usually do the frontend and backend in two separate panes in a tmux window.

Generate the list of llvm libs that need to be linked with: 
`llvm-config --libs --system-libs --ldflags --link-static > llvm_flags.sexp`