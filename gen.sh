#!/bin/bash

echo "Profiling..."
stack exec -- profile $GITHUB_WORKSPACE
