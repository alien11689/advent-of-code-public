#!/bin/sh

year=2015

../mvnw clean package -am -pl $year -f ../pom.xml > /dev/null
java -jar target/${year}-1.0.0-SNAPSHOT-jar-with-dependencies.jar
