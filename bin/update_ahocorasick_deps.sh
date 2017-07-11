#!/usr/bin/env bash

DIR=$( dirname $BASH_SOURCE[0] )

rm $DIR/../inst/java/*.jar

# the output directory should be `$DIR/../inst/java/hive/*.jar` as the argument of `rm` above, but on this "wrong" path
# works...
mvn dependency:copy-dependencies -DoutputDirectory=$DIR/../inst/java -f $DIR/ahocorasickjava.pom


