#!/bin/bash

clear;

echo "________________________hlint________________________"
find ./src ./app ./test | rg '.hs' | entr -c -r ./lint.sh
