#!/usr/bin/env bash
clang -Xclang -load -Xclang build/skeleton/libSkeletonPass.so test.c
