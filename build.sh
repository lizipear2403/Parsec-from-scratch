#!/bin/sh 

ghc ./JSONParser.hs \
    ./ParserCombinator.hs \
    ./Main.hs \
    ./Helper.hs \
    ./StateMonad.hs  \
    -odir  ./build/ofiles/ \
    -hidir ./build/hifiles/ \
    -o ./build/main/main \
     -Wall 
