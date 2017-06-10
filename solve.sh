#!/usr/bin/bash
scalac -sourcepath src -d out -cp out src/adventofcode/${1}.scala && \
echo 'Running...' && \
scala -cp out adventofcode.${1}_Solution
