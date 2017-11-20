#!/bin/bash

STACKARG=""

if [ "$STACK_IN_CONTAINER" == "1" ]; then
  STACKARG="--stack-yaml=stack-static.yaml"
fi

stack $STACKARG exec test-preproc -- "$@"
