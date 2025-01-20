package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;

import java.util.stream.Stream;

class All {
    public static void main(String[] args) {
        var days = Stream.of(
                new Day01(),
                new Day02(),
                new Day03(),
                new Day04(),
                new Day05(),
                new Day06(),
                new Day07(),
                new Day08(),
                new Day09(),
                new Day10(),
                new Day11(),
                new Day12(),
                new Day13(),
                new Day14(),
                new Day15(),
                new Day16(),
                new Day17(),
                new Day18(),
                new Day19(),
                new Day20(),
                new Day21(),
                new Day22(),
                new Day23(),
                new Day24(),
                new Day25()
        );
        if (args.length > 0) {
            days = days.filter(d -> d.dayNum() == Integer.parseInt(args[0]));
        }
        Stream<Day> finalDays = days;
        Util.measureTime(() -> finalDays
                .forEach(d -> {
                    System.out.printf("Day%02d%n", d.dayNum());
                    Util.measureTime(d::execute);
                }));
    }
}
