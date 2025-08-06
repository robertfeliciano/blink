# Welcome to Blink!

Blink aims to be a GPU accelerated programming language focused on machine learning algorithms.

### How to build this project

One time I made the mistake of not touching this project for 2 months and I completely forgot how to compile it. 

Starting in the root directory, `blink/`, run the following commands: 

`cd backend`

`./builder`

`cd build`

`make`

`cd ../../frontend`

`dune build`

`dune exec src/blink.exe -- <filename>`

I usually do the frontend and backend in two separate panes in a tmux window.