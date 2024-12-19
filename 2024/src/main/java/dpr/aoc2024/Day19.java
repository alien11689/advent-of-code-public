package dpr.aoc2024;

import dpr.commons.Util;
import kotlin.Pair;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

class Day19 implements Day {
    public static void main(String... args) {
        new Day19().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 19;
    }

    private Object part1(List<String> lines) {
        List<String> towels = Arrays.stream(lines.get(0).split(", ")).sorted().toList();
        int count = 0;
        for (int i = 1; i < lines.size(); ++i) {
            String design = lines.get(i);
//            System.out.println("Checking " + design);
            if (findMatch(towels, design)) {
                ++count;
            }
        }
        return count;
    }

    private static boolean findMatch(List<String> towels, String design) {
        PriorityQueue<Integer> pq = new PriorityQueue<>(new Comparator<Integer>() {
            @Override
            public int compare(Integer o1, Integer o2) {
                return o2.compareTo(o1);
            }
        });
        pq.offer(0);
        Set<Integer> checked = new HashSet<>();
        Set<String> possibleTowels = towels.stream().filter(t -> design.contains(t)).collect(Collectors.toSet());
        while (!pq.isEmpty()) {
            int cur = pq.poll();
            if (checked.contains(cur)) {
                continue;
            }
            checked.add(cur);
//            System.out.println(design + ": " + pq.size() + ", cur is " + cur + "/" + design.length());
            for (String t : possibleTowels) {
                if (design.startsWith(t, cur)) {
                    int next = cur + t.length();
                    if (next == design.length()) {
                        return true;
                    }
                    if (!checked.contains(next)) {
                        pq.offer(next);
                    }
                }
            }
        }
        return false;
    }

    private Object part2(List<String> lines) {
        List<String> towels = Arrays.stream(lines.get(0).split(", ")).sorted().toList();
        long count = 0;
        for (int i = 1; i < lines.size(); ++i) {
            String design = lines.get(i);
//            System.out.println("Checking " + design);
            count += findPossible(towels, design);
        }
        return count;
    }

    record Match(int idx, int count) {
    }

    private static long findPossible(List<String> towels, String design) {
        Set<String> possibleTowels = towels.stream().filter(t -> design.contains(t)).collect(Collectors.toSet());
        Set<Pair<Integer, Integer>> passes = new HashSet<>();
        for (int i = 0; i < design.length(); ++i) {
            for (String t : possibleTowels) {
                if (design.startsWith(t, i)) {
                    passes.add(new Pair<Integer, Integer>(i, i + t.length()));
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
            Set<Pair<Integer, Integer>> available = passes.stream().filter(p -> p.getFirst() == cur).collect(Collectors.toSet());
            available.stream().map(p -> p.getSecond()).forEach(target -> {
                targetToWays.compute(target, (k, v) -> v == null ? ways : (v + ways));
                pq.offer(target);
            });
            passes.removeAll(available);
        }
//        System.out.println(targetToWays);
        return targetToWays.getOrDefault(design.length(), 0L);
    }
}
