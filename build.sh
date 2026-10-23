#!/bin/sh 

ghc ./ParserMonad.hs \
    ./ParserCombinator.hs \
    ./StateMonad.hs  \
    -odir  ./build/ofiles/ \
    -hidir ./build/hifiles/ \
    -o ./build/main/main
