#!/bin/bash

echo "________________________start________________________"
find ./src ./app | grep .hs$ | entr -c -r ./start.sh

