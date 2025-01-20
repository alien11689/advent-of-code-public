package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Util;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

class Day19 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        Pair<Object, Object> solution1And2 = part1And2(lines);
        System.out.println(solution1And2.first());
        System.out.println(solution1And2.second());
    }

    @Override
    public int dayNum() {
        return 19;
    }

    Pair<Object, Object> part1And2(List<String> lines) {
        List<String> towels = Arrays.stream(lines.getFirst().split(", ")).sorted().toList();
        int part1 = 0;
        long part2 = 0;
        for (int i = 1; i < lines.size(); ++i) {
            String design = lines.get(i);
            long possible = findPossible(towels, design);
            if (possible > 0) {
                ++part1;
            }
            part2 += possible;
        }
        return new Pair<>(part1, part2);
    }

    private static long findPossible(List<String> towels, String design) {
        Set<String> possibleTowels = towels.stream().filter(design::contains).collect(Collectors.toSet());
        Set<Pair<Integer, Integer>> passes = new HashSet<>();
        for (int i = 0; i < design.length(); ++i) {
            for (String t : possibleTowels) {
                if (design.startsWith(t, i)) {
                    passes.add(new Pair<>(i, i + t.length()));
                }
            }
        }
//        System.out.println(passes);
        Map<Integer, Long> targetToWays = new HashMap<>();
        targetToWays.put(0, 1L);
        int min = -1;
        PriorityQueue<Integer> pq = new PriorityQueue<>();
        pq.offer(0);
        while (!pq.isEmpty()) {
            int cur = pq.poll();
            if (cur <= min) {
                continue;
            }
            min = cur;
            long ways = targetToWays.get(cur);
            Set<Pair<Integer, Integer>> available = passes.stream().filter(p -> p.first() == cur).collect(Collectors.toSet());
            available.stream().map(Pair::second).forEach(target -> {
                targetToWays.compute(target, (k, v) -> v == null ? ways : (v + ways));
                pq.offer(target);
            });
            passes.removeAll(available);
        }
//        System.out.println(targetToWays);
        return targetToWays.getOrDefault(design.length(), 0L);
    }
}
