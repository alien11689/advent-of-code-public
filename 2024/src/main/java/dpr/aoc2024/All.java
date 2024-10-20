package dpr.aoc2024;

import dpr.commons.Util;

import java.lang.reflect.InvocationTargetException;
import java.util.stream.Stream;

class All {
    public static void main(String[] args) throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        var days = Stream.of(
                new Day01()
        );
        if (args.length > 0) {
            days = days.filter(d -> d.dayNum() == Integer.parseInt(args[0]));
        }
        Stream<Day01> finalDays = days;
        Util.measureTime(() -> finalDays
                .forEach(d -> {
                    System.out.printf("Day%02d%n", d.dayNum());
                    d.execute(args);
                }));
    }
}
