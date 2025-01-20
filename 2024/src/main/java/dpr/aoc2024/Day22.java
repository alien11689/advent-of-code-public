package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Util;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

class Day22 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        Pair<Object, Object> solution1And2 = part1And2(lines);
        System.out.println(solution1And2.first());
        System.out.println(solution1And2.second());
    }

    @Override
    public int dayNum() {
        return 22;
    }

    Pair<Object, Object> part1And2(List<String> lines) {
        Map<String, Integer> prices = new ConcurrentHashMap<>();
        long part1 = lines.stream().parallel().mapToLong(line -> calculate(Long.parseLong(line), 2000, prices)).sum();
        int part2 = prices.values().stream().mapToInt(l -> l).max().orElse(Integer.MIN_VALUE);
        return new Pair<>(part1, part2);
    }

    private long calculate(long init, int iter, Map<String, Integer> prices) {
        long cur = init;
        int last = (int) (cur % 10);
        Queue<Integer> q = new LinkedList<>();
        Set<String> seen = new HashSet<>();
        for (int i = 1; i <= iter; i++) {
            cur = (cur ^ (cur << 6)) % 16777216;
            cur = (cur ^ (cur >> 5)) % 16777216;
            cur = (cur ^ (cur << 11)) % 16777216;
            int newlast = (int) (cur % 10);
            if (q.size() == 4) {
                q.poll();
            }
            q.add(newlast - last);
            String key = q.toString();
            if (!seen.contains(key) && q.size() == 4) {
                prices.compute(key, (k, v) -> v == null ? newlast : v + newlast);
                seen.add(key);
            }
//            System.out.println(init + " in iteration " + i + " " + key + " -> " + newlast);
            last = newlast;
        }
        return cur;
    }
}
