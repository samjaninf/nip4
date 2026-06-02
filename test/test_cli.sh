#!/bin/bash

set -e

snip=$1
exit=0

check_output() {
  output=$($snip -e "$1")
  if [ "$output" != "$2" ]; then
    echo FAIL: snip output was $output should be $2
    exit=1
  fi
}

check_output '2 + 2' 4
check_output '"hello"' hello
check_output '"2" ++ "hello"' 2hello
check_output '"hello\n"' hello
check_output '[1..5]' '[1, 2, 3, 4, 5]'

exit $exit

