package dpr.aoc2024;

import dpr.commons.Util;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

class Day07 implements Day {
    public static void main(String... args) {
        new Day07().execute(args);
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
        return 7;
    }

    private Object part1(List<String> lines) {
        return lines.stream()
                .parallel()
                .mapToLong(l -> calculatePart1(l, false))
                .sum();
    }

    record Partial(long first, List<Long> rest) {
    }

    private long calculatePart1(String line, boolean part2) {
//        System.out.println("Checking "+ line);
        String[] parts = line.split(": ");
        long target = Long.parseLong(parts[0]);
        List<Long> values = Arrays.stream(parts[1].split(" "))
                .map(Long::parseLong)
                .toList();
        Queue<Partial> q = new LinkedList<>();
        q.offer(new Partial(values.getFirst(), values.stream().skip(1).toList()));
        while (!q.isEmpty()) {
            Partial p = q.poll();
//            System.out.println("Checking "+ p + " for " + target);
            if (p.first > target) {
                continue;
            }
            if (p.rest.isEmpty()) {
                if (p.first == target) {
                    return target;
                }
                continue;
            }
            long next = p.rest.getFirst();
            List<Long> nextRest = p.rest.stream().skip(1).toList();
            q.offer(new Partial(p.first + next, nextRest));
            q.offer(new Partial(p.first * next, nextRest));
            if (part2) {
                q.offer(new Partial(Long.parseLong(p.first + "" + next), nextRest));
            }
        }
        return 0L;
    }

    private Object part2(List<String> lines) {
        return lines.stream()
                .parallel()
                .mapToLong(l -> calculatePart1(l, true))
                .sum();
    }

}
