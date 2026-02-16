#!/bin/bash

for test in *.def; do
  echo -n "$test: "
  snip --test $test
done

