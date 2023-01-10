#!/bin/bash

echo "Profiling..."
stack exec -- profile $GITHUB_WORKSPACE
echo "Generating flame graphs..."
find $GITHUB_WORKSPACE -name "*.prof" -exec ~/.local/bin/ghc-prof-flamegraph --alloc $f -o $GITHUB_WORKSPACE/results/$f-alloc.svg \;
find $GITHUB_WORKSPACE -name "*.prof" -exec ~/.local/bin/ghc-prof-flamegraph --time $f -o $GITHUB_WORKSPACE/results/$f-time.svg \;
