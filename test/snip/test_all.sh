#!/bin/bash

# script use this to find their samples
export IMAGES="./images"

for test in *.def; do
  echo -n "$test: "
  snip --test $test images/slanted_oval_vase2.jpg
done

