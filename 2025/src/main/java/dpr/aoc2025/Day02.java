package dpr.aoc2025;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import dpr.commons.Day;
import dpr.commons.Util;

class Day02 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 2;
    }

    long part1(List<String> lines) {
        return Arrays.stream(lines.get(0).split(","))
                .map(p -> p.split("-"))
                .mapToLong(p -> checkRange1(Long.parseLong(p[0]), Long.parseLong(p[1])))
                .sum();
    }

    private long checkRange1(long left, long right) {
        long sum = 0;
        long cur = left;
        while (cur <= right) {
            String s = String.valueOf(cur);
            if (s.length() % 2 == 0) {
                var begin = s.substring(0, s.length() / 2);
                var end = s.substring(s.length() / 2);
                if (begin.equals(end)) {
                    sum += cur;
                }
            }
            ++cur;
        }
        return sum;
    }

    long part2(List<String> lines) {
        return Arrays.stream(lines.get(0).split(","))
                .map(p -> p.split("-"))
                .mapToLong(p -> checkRange2(Long.parseLong(p[0]), Long.parseLong(p[1])))
                .sum();
    }

    private long checkRange2(long left, long right) {
        long sum = 0;
        long cur = left;
        Set<String> found = new HashSet<>();
        while (cur <= right) {
            String s = String.valueOf(cur);
            int maxDigits = s.length();
            int substringSize = 1;
            while (substringSize <= maxDigits / 2) {
                var begin = s.substring(0, substringSize);
                StringBuilder sb = new StringBuilder(begin);
                while (sb.length() + substringSize <= maxDigits) {
                    sb.append(begin);
                }
                if (sb.toString().equals(s)) {
                    if(!found.contains(s)) {
                        found.add(s);
                        sum += cur;
                    }
                }
                ++substringSize;
            }
            ++cur;
        }
        return sum;
    }

    public static void main(String[] args) {
        new Day02().execute();
    }
}
