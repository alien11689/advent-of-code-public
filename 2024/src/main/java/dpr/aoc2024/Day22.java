package dpr.aoc2024;

import dpr.commons.Util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

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
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test3.txt", dayNum()));
            part1And2(lines);
        });
    }

    @Override
    public int dayNum() {
        return 22;
    }

    private void part1And2(List<String> lines) {
        long part1 = 0;
        Map<String, Integer> prices = new HashMap<>();
        for (String line : lines) {
            long init = Long.parseLong(line);
            part1 += calculate(init, 2000, prices);
        }
        System.out.println(part1);
        long part2 = prices.values().stream().mapToLong(l -> l).max().orElse(Long.MIN_VALUE);
        System.out.println(part2);
        ;
    }

    private long calculate(long init, int iter, Map<String, Integer> prices) {
        long cur = init;
        long last = cur % 10;
        Queue<Long> q = new LinkedList<>();
        Set<String> seen = new HashSet<>();
        for (int i = 1; i <= iter; i++) {
            cur = (cur ^ (cur << 6)) % 16777216;
            cur = (cur ^ (cur >> 5)) % 16777216;
            cur = (cur ^ (cur << 11)) % 16777216;
            long newlast = cur % 10;
            if (q.size() == 4) {
                q.poll();
            }
            q.add(newlast - last);
            String key = q.toString();
            if (!seen.contains(key) && q.size() == 4) {
                prices.put(key, prices.getOrDefault(key, 0) + (int) newlast);
                seen.add(key);
            }
//            System.out.println(init + " in iteration " + i + " " + key + " -> " + newlast);
            last = newlast;
        }
        return cur;
    }
}
