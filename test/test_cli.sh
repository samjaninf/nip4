#!/bin/bash

set -e
#set -x

snip=$1
exit=0

check_workspace_output() {
  cmd="$1"
  expected="$2"

  echo -n "snip -e $cmd: "
  output=$($snip -w $cmd)
  pass=$($snip -e "abs ($output - $expected) < 0.1")
  if [ "$pass" != "true" ]; then
    echo FAIL: snip output was $output should be $expected
    exit=1
  else
    echo pass
  fi
}

check_output() {
  cmd="$1"
  expected="$2"

  echo -n "snip -e $cmd: "
  output=$($snip -e "$cmd")
  if [ "$output" != "$expected" ]; then
    echo FAIL: snip output was $output should be $expected
    exit=1
  else
    echo pass
  fi
}

check_output '2 + 2' 4

check_output '"hello"' hello
check_output '"2" ++ "hello"' 2hello
check_output '"hello\n"' hello

check_output "['a', 'b', 'c']" abc
check_output "[1, 2, 3, 'a', 'b', 'c']" "[1, 2, 3, 'a', 'b', 'c']"
check_output "['a', 'b', 'c', 1, 2, 3]" "['a', 'b', 'c', 1, 2, 3]"
check_output "['a', 'b', 'c', 'd', 1, 2, 3]" "abcd ++ [1, 2, 3]"

check_output '[1..5]' '[1, 2, 3, 4, 5]'

check_workspace_output '../test/test_cli.ws ../test/test_cli.def' 111.4996
check_workspace_output '../test/test_cli.ws --set main=Workspaces.tab2.A3' 111.4996

rm -f test/fred.jpg
echo -n "snip -w save jpg from workspace: "
$snip -w ../test/test_cli.ws --set main=Workspaces.tab2.A2 -o test/fred.jpg
if [ ! -f test/fred.jpg ]; then
  echo FAIL
  exit=1
else
  echo pass
fi

rm -f test/fred.jpg
echo -n "snip -w save jpg with options from workspace: "
$snip -w ../test/test_cli.ws --set main=Workspaces.tab2.A2 -o test/fred.jpg[Q=45]
if [ ! -f test/fred.jpg ]; then
  echo FAIL
  exit=1
else
  echo pass
fi

rm -f test/fred.jpg
rm -f test/fred1.jpg
echo -n "snip -w save two jpgs from workspace: "
$snip -w ../test/test_cli.ws --set main=[Workspaces.tab2.A2,Workspaces.tab2.A2] -o test/fred.jpg
if [ ! -f test/fred.jpg -o ! -f test/fred1.jpg ]; then
  echo FAIL
  exit=1
else
  echo pass
fi

# can save two jpgs from a workspace with middle numbering
rm -f test/fred_012_crop.jpg
rm -f test/fred_013_crop.jpg
echo -n "snip -w middle save filename increment: "
$snip -w ../test/test_cli.ws --set main=[Workspaces.tab2.A2,Workspaces.tab2.A2] -o test/fred_012_crop.jpg
if [ ! -f test/fred_012_crop.jpg -o ! -f test/fred_013_crop.jpg ]; then
  echo FAIL
  exit=1
else
  echo pass
fi

exit $exit

