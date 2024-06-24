#!/bin/bash

# Run tests for .shr files in ./bad directory
for file in ./bad/*.shr; do
    test_name=$(basename "$file" .shr)
    touch "./TestResources/Out/$test_name.out" "./TestResources/Err/$test_name.err"
    ./interpreter "$file" > "./TestResources/Out/$test_name.out" 2> "./TestResources/Err/$test_name.err"
done

# Run tests for .shr files in ./good directory
for file in ./good/*.shr; do
    test_name=$(basename "$file" .shr)
    touch "./TestResources/Out/$test_name.out" "./TestResources/Err/$test_name.err"
    ./interpreter "$file" > "./TestResources/Out/$test_name.out" 2> "./TestResources/Err/$test_name.err"
done