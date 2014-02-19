#!/usr/bin/env sh
set -e

echo "Running backend tests"
sbt test
