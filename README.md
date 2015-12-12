# Automate
Gröbner basis calculations in Scheme

## Prerequisites
This package is written in portable R7RS Scheme. Its Makefile, however, assumes an installation of Chibi Scheme in the default path. So it is best to locally install [Chibi Scheme](https://github.com/ashinn/chibi-scheme) first.

## Run the tests
    make test

## Enter the REPL

    $ make repl
    chibi-scheme -Ilib -R -mautomate
    > (import (automate rational-field))
    > (import (automate grevlex-ordering))
    > (define-algebra a (
    > (define-algebra a (rational-field grevlex-ordering (x y z)
                                        ((+ (4 z) (-4 x y 2) (-16 x 2) -1)
                                         (+ (2 y 2 z) (4 x) 1)
                                         (+ (2 x 2 z) (2 y 2) x))))
    > (a (+ (4 x y 4) (16 x 2 y 2) (y 2) (8 x) 2))
    0
