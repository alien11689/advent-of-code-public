package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class Day07 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 7;
    }

    Object part1(List<String> lines) {
        return lines.stream()
                .parallel()
                .mapToLong(l -> calculate(l, false))
                .sum();
    }

    record Partial(long first, int idx) {
    }

    private long calculate(String line, boolean part2) {
//        System.out.println("Checking "+ line);
        String[] parts = line.split(": ");
        long target = Long.parseLong(parts[0]);
        List<Long> values = Arrays.stream(parts[1].split(" "))
                .map(Long::parseLong)
                .toList();
        Queue<Partial> q = new LinkedList<>();
        q.offer(new Partial(values.getFirst(), 1));
        while (!q.isEmpty()) {
            Partial p = q.poll();
//            System.out.println("Checking "+ p + " for " + target);
            if (p.first > target) {
                continue;
            }
            if (p.idx >= values.size()) {
                if (p.first == target) {
                    return target;
                }
                continue;
            }
            long next = values.get(p.idx);
            q.offer(new Partial(p.first + next, p.idx + 1));
            q.offer(new Partial(p.first * next, p.idx + 1));
            if (part2) {
                q.offer(new Partial(Long.parseLong(p.first + "" + next), p.idx + 1));
            }
        }
        return 0L;
    }

    Object part2(List<String> lines) {
        return lines.stream()
                .parallel()
                .mapToLong(l -> calculate(l, true))
                .sum();
    }

}
