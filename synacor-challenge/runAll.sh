#!/bin/bash

module=synacor-challenge

../mvnw clean package -am -pl $module -f ../pom.xml > /dev/null
java -Xss100m -jar target/$module-1.0.0-SNAPSHOT-jar-with-dependencies.jar < actions
