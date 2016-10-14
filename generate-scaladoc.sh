#!/bin/sh

sbt doc
cp -r target/scala-2.11/api/* docs/

