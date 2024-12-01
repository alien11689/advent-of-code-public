package dpr.aoc2024;

import dpr.commons.Util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

class Day01 implements Day {
    public static void main(String... args) {
        new Day01().execute(args);
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
        return 1;
    }

    private Object part1(List<String> lines) {
        var lefts = new ArrayList<Integer>();
        var rights = new ArrayList<Integer>();
        lines.forEach(line -> {
            var parts = line.split(" +");
            int left = Integer.parseInt(parts[0]);
            int right = Integer.parseInt(parts[1]);
            lefts.add(left);
            rights.add(right);
        });
        Collections.sort(lefts);
        Collections.sort(rights);
        var res = 0;
        for(int i = 0; i < lefts.size(); i++) {
            res += Math.abs(lefts.get(i) - rights.get(i));
        }
        return res;
    }

    private Object part2(List<String> lines) {
        var lefts = new ArrayList<Integer>();
        var rights = new ArrayList<Integer>();
        lines.forEach(line -> {
            var parts = line.split(" +");
            int left = Integer.parseInt(parts[0]);
            int right = Integer.parseInt(parts[1]);
            lefts.add(left);
            rights.add(right);
        });
        var res = 0L;
        for (int left : lefts) {
            long count = rights.stream().filter(r -> r.equals(left)).count();
            res += left * count;
        }
        return res;
    }
}
