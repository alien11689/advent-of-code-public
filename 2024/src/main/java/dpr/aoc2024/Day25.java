package dpr.aoc2024;

import dpr.commons.Point2D;
import dpr.commons.Util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

class Day25 implements Day {
    public static void main(String... args) {
        new Day25().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
            System.out.println(part1(lines));
        });
    }

    @Override
    public int dayNum() {
        return 25;
    }

    private Object part1(List<String> lines) {
        Set<List<Integer>> keys = new HashSet<>();
        Set<List<Integer>> locks = new HashSet<>();

        Set<Point2D> curBlocks = new HashSet<>();
        int y = 0;
        boolean lock = false;
        for (String line : lines) {
            if (line.isBlank()) {
                List<Integer> columns = columns(curBlocks);
                if (lock) {
//                    System.out.println("Adding " + columns + " to locks");
                    locks.add(columns);
                } else {
//                    System.out.println("Adding " + columns + " to keys");
                    keys.add(columns);
                }
                y = 0;
                lock = false;
                curBlocks.clear();
                continue;
            }
            for (int x = 0; x < line.length(); x++) {
                if (line.charAt(x) == '#' && y == 0) {
                    lock = true;
                }
                if (line.charAt(x) == '#') {
                    curBlocks.add(new Point2D(x, y));
                }
                ++y;
            }
        }
        int count = 0;
        for (List<Integer> k : keys) {
            ll:
            for (List<Integer> l : locks) {
                for (int i = 0; i < 5; ++i) {
                    if (k.get(i) + l.get(i) > 5) {
                        continue ll;
                    }
                }
                ++count;
            }
        }
        return count;
    }

    private List<Integer> columns(Set<Point2D> curBlocks) {
        List<Integer> columns = new ArrayList<>();
        for (int i = 0; i < 5; ++i) {
            int finalI = i;
            columns.add((int) curBlocks.stream().filter(p -> p.getX() == finalI).count() - 1);
        }
        return columns;
    }
}
