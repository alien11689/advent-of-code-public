package dpr.aoc2024;

import dpr.commons.Util;

import java.util.List;

class Day25 implements Day {
    public static void main(String... args) {
        new Day25().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
            System.out.println(part1(lines));
        });
    }

    @Override
    public int dayNum() {
        return 25;
    }

    private Object part1(List<String> lines) {
        return null;
    }
}
