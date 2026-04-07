#!/bin/bash

should_pass="\
  test_colour.ws \
  test_filter.ws \
  test_histogram.ws \
  test_image.ws \
  test_magick.ws \
  test_math.ws \
  test_matrix.ws \
  test_region.ws \
  test_tasks.ws \
  test_widgets.ws \
"

should_fail="\
  test_fail.ws \
"

for test in $should_pass; do
  echo -n "$test: "
  if ! snip --test --workspace $test; then
    echo FAIL
    exit -1
  fi
  echo ok
done

for test in $should_fail; do
  echo -n "$test should fail: "
  if snip --test --workspace $test; then
    echo FAIL
    exit -1
  fi
  echo ok
done

echo all tests passed successfully
