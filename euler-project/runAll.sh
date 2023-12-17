#!/bin/bash

module=euler-project

../mvnw clean package -am -pl $module -f ../pom.xml > /dev/null
java -jar target/$module-1.0.0-SNAPSHOT-jar-with-dependencies.jar
