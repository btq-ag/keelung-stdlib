#!/bin/bash

stack exec -- profile $GITHUB_WORKSPACE
find $GITHUB_WORKSPACE -name "*.prof" -exec ~/.local/bin/ghc-prof-flamegraph --alloc $f -o $GITHUB_WORKSPACE/results/$f-alloc.svg \;
find $GITHUB_WORKSPACE -name "*.prof" -exec ~/.local/bin/ghc-prof-flamegraph --time $f -o $GITHUB_WORKSPACE/results/$f-time.svg \;
