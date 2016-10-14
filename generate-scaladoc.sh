#!/bin/sh

sbt doc
cp -r target/scala-2.11/api/* docs/

echo "run: find docs/ -type f -exec perl -i'' -pE \"s/$(pwd)//g\" {} \;"
echo "remember to escape slashes in regular expression"

