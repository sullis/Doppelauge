#!/bin/sh

sbt doc
scp -r target/scala-2.11/api ksvalast@login.uio.no:www_docs/doppelauge/
