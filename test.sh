#!/bin/bash

OK="\e[32mOK\e[0m"
ERROR="\e[31mERROR\e[0m"
clear
echo "========================== Compiling =========================================="

make all

if [ $? -ne 0 ]; then
    echo "Failed to compile."
    exit 1
fi


make typeCheckerTest

if [ $? -ne 0 ]; then
    echo "typeCheckerTest failed to compile."
    exit 1
fi

echo "========================== Running TypeChecker tests =========================="


./typeCheckerTest

if [ $? -ne 0 ]; then
    echo "typeCheckerTest failed"
    exit 1
fi

echo -e $OK


echo "========================== checking bad files ================================="
for file in bad/*.shr; do
    echo -n "$file: "
    ./interpreter $file > tmp.out 2> tmp.err
    if [ $? -eq 0 ]; then
        echo -e "$ERROR - Test should have failed but didn't"
        rm -rf tmp.err tmp.out
        exit 1
    fi

    diff tmp.err TestResources/Err/$(basename $file .shr).err > /dev/null
    if [ $? -ne 0 ]; then
        echo -e "$ERROR - Stderr output does not match expected output"
        rm -rf tmp.err tmp.out
        exit 1
    fi
    diff tmp.out TestResources/Out/$(basename $file .shr).out > /dev/null
    if [ $? -ne 0 ]; then
        echo -e "$ERROR - Stdout output does not match expected output"
        rm -rf tmp.err tmp.out
        exit 1
    fi
    echo -e $OK
done

echo "========================== checking good files ================================"
for file in good/*.shr; do
    echo -n "$file: "
    ./interpreter $file > tmp.out 2> tmp.err
    if [ $? -ne 0 ]; then
        echo -e "\n$ERROR - Test should have passed but didn't"
        rm -rf tmp.err tmp.out
        exit 1
    fi

    diff tmp.err TestResources/Err/$(basename $file .shr).err > /dev/null
    if [ $? -ne 0 ]; then
        echo -e "\n$ERROR - Stderr output does not match expected output"
        rm -rf tmp.err tmp.out
        exit 1
    fi
    diff tmp.out TestResources/Out/$(basename $file .shr).out > /dev/null
    if [ $? -ne 0 ]; then
        echo -e "\n$ERROR - Stdout output does not match expected output"
        rm -rf tmp.err tmp.out
        exit 1
    fi

    echo -e "$OK"
done

rm -rf tmp.err tmp.out
echo -e "\e[32mAll tests passed\e[0m"

