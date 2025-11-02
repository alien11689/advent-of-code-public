package dpr.aoc2025;

import java.util.stream.Stream;

import dpr.commons.Day;
import dpr.commons.Util;

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
                new Day12()
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
