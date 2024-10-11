#!/bin/bash

# Feature combinations to test
features=(
    ""
    "c_stdlib"
    "repl"
    "c_stdlib repl"
)

for feature_set in "${features[@]}"; do
    if [ -z "$feature_set" ]; then
        echo "Running tests without any features..."
        cargo test --verbose
    else
        echo "Running tests with features: $feature_set..."
        cargo test --verbose --features "$feature_set"
    fi

    if [ $? -ne 0 ]; then
        echo "Tests failed for features: $feature_set"
        exit 1
    fi
done

echo "All feature combinations passed successfully."
