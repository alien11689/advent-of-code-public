package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class Day02 implements Day {
    public static void main(String... args) {
        new Day02().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 2;
    }

    private Object part1(List<String> lines) {
        return lines.stream()
                .map(line -> Arrays.stream(line.split(" +")).map(Integer::parseInt).toList())
                .filter(this::isSafe)
                .count();
    }

    private boolean isSafe(List<Integer> numbers) {
        List<Integer> diffs = new ArrayList<>();
        for (int i = 1; i < numbers.size(); i++) {
            diffs.add(numbers.get(i) - numbers.get(i - 1));
        }
        return (diffs.stream().allMatch(i -> i > 0) || diffs.stream().allMatch(i -> i < 0)) && diffs.stream().allMatch(i -> {
            var a = Math.abs(i);
            return a == 1 || a == 2 || a == 3;
        });
    }

    private Object part2(List<String> lines) {
        return lines.stream()
                .map(line -> Arrays.stream(line.split(" +")).map(Integer::parseInt).toList())
                .filter(this::isSafeWithDamper)
                .count();
    }

    private boolean isSafeWithDamper(List<Integer> numbers) {
        if (isSafe(numbers)) {
            return true;
        }
        for (int i = 0; i < numbers.size(); i++) {
            var cur = new ArrayList<>(numbers);
            cur.remove(i);
            if (isSafe(cur)) {
                return true;
            }
        }
        return false;
    }
}
