package dpr.aoc2025;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.jetbrains.annotations.NotNull;

import dpr.commons.Day;
import dpr.commons.Util;

public class Day05 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 5;
    }

    int part1(List<String> lines) {
        List<Range> ranges = new ArrayList<>();
        int fresh = 0;
        for (String line : lines) {
            if (line.contains("-")) {
                String[] split = line.split("-");
                ranges.add(new Range(Long.parseLong(split[0]), Long.parseLong(split[1])));
            } else {
                long cur = Long.parseLong(line);
                if (ranges.stream().anyMatch(p -> p.contains(cur))) {
                    fresh++;
                }
            }
        }
        return fresh;
    }

    long part2(List<String> lines) {
        List<Range> ranges = readSortedRanges(lines);
        long res = sumUniqueIngredientsInRanges(ranges);

        return res;
    }

    private static long sumUniqueIngredientsInRanges(List<Range> ranges) {
        long res = 0;
        var prev = ranges.getFirst();
        for (int i = 1; i < ranges.size(); i++) {
            var cur = ranges.get(i);
            if (prev.overlap(cur)) {
                prev = prev.extend(cur);
            } else {
                res += prev.size();
                prev = cur;
            }
        }
        res += prev.size();
        return res;
    }

    @NotNull
    private static List<Range> readSortedRanges(List<String> lines) {
        List<Range> ranges = new ArrayList<>();
        for (String line : lines) {
            if (line.contains("-")) {
                String[] split = line.split("-");
                ranges.add(new Range(Long.parseLong(split[0]), Long.parseLong(split[1])));
            }
        }
        Collections.sort(ranges);
        return ranges;
    }

    record Range(long lower, long upper) implements Comparable<Range> {
        boolean contains(long value) {
            return value >= lower && value <= upper;
        }

        @Override
        public int compareTo(@NotNull Range o) {
            int compare = Long.compare(lower, o.lower);
            if (compare == 0) {
                return Long.compare(upper, o.upper);
            }
            return compare;
        }

        public long size() {
            return upper - lower + 1;
        }

        public Range extend(Range next) {
            if (!contains(next.lower)) {
                throw new IllegalArgumentException();
            }
            return new Range(lower, contains(next.upper) ? upper : next.upper);
        }

        public boolean overlap(Range cur) {
            return contains(cur.lower);
        }
    }

    public static void main(String[] args) {
        new Day05().execute();
    }
}
