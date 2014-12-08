ocaml-markov
============

Markov chain generator written in OCaml.

I wanted to write something easy but substantial to learn OCaml, so I wrote a simple order-2 Markov chain generator. It's pretty naïve (there are some glaring problems and opportunities for optimization), but at the moment it works and I learned a lot, which is what I was going for.

Running the generator is simple enough. Because it reads lines from stdin, you can just redirect a text file to it and it'll
use it as input:

```
./markov < corpus.txt
```
