#!/bin/sh

module=$1

./mvnw clean package -am -pl $module > /dev/null
if [ "x$module" == "xsynacor-challenge" ]; then
  java -Xss100m -jar target/${module}-1.0.0-SNAPSHOT-jar-with-dependencies.jar < $x/actions
else
  java -jar $module/target/${module}-1.0.0-SNAPSHOT-jar-with-dependencies.jar
fi
