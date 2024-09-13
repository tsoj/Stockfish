#!/bin/bash

cd src
make build ARCH=x86-64 COMP=clang
cd ..
./src/stockfish
