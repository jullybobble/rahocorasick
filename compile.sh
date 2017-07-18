#! /usr/bin/env bash

declare -r DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p ${DIR}/target/java
javac -cp ${DIR}/inst/java/\* -d ${DIR}/target/java/ ${DIR}/src/java/ahocorasick/*.java
jar cf ${DIR}/inst/java/ahocorasick.jar -C ${DIR}/target/java ahocorasick
