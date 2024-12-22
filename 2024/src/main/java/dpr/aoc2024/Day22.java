package dpr.aoc2024;

import dpr.commons.Util;

import java.util.List;

class Day22 implements Day {
    public static void main(String... args) {
        new Day22().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test2.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 22;
    }

    private Object part1(List<String> lines) {
        long sum = 0;
        for (String line : lines) {
            long init = Long.parseLong(line);
            long secret = calculatePseudo(init, 2000);
            System.out.println("For " + init + " secret: " + secret);
            sum += secret;
        }
        return sum;
    }

    private long calculatePseudo(long init, int iter) {
        long cur = init;
        for (int i = 1; i <= iter; i++) {
            cur = (cur ^ (cur << 6)) % 16777216;
            cur = (cur ^ (cur >> 5)) % 16777216;
            cur = (cur ^ (cur << 11)) % 16777216;
        }
        return cur;
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
