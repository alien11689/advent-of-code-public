package dpr.aoc2024;

import dpr.commons.Point2D;
import dpr.commons.Util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

class Day20 implements Day {
    public static void main(String... args) {
        new Day20().execute(args);
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
        return 20;
    }

    private Object part1(List<String> lines) {
        return countCheats(lines, 2);
    }

    private Object part2(List<String> lines) {
        return countCheats(lines, 20);
    }

    private static long countCheats(List<String> lines, int limit) {
        Set<Point2D> blocks = new HashSet<>();
        Point2D start = new Point2D(0, 0);
        Point2D end = new Point2D(0, 0);
        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            for (int x = 0; x < line.length(); x++) {
                Point2D p = new Point2D(x, y);
                char c = line.charAt(x);
                if (c == '#') {
                    blocks.add(p);
                } else if (c == 'S') {
                    start = p;
                } else if (c == 'E') {
                    end = p;
                }
            }
        }

        Map<Point2D, Integer> path = new HashMap<>();
        int length = 0;
        path.put(start, length);
        Queue<Point2D> q = new LinkedList<>();
        q.offer(start);
        while (!q.isEmpty()) {
            if (q.size() > 1) {
                throw new RuntimeException();
            }
            Point2D cur = q.poll();
            path.put(cur, length++);
            List<Point2D> neighbours = cur.neighboursCross();
            for (Point2D n : neighbours) {
                if (!blocks.contains(n) && !path.containsKey(n)) {
                    q.offer(n);
                }
            }
        }

        long count = 0;
        for (Map.Entry<Point2D, Integer> pos : path.entrySet()) {
            for (Map.Entry<Point2D, Integer> other : path.entrySet()) {
                if (other.getValue() <= pos.getValue()) {
                    continue;
                }
                int manhattan = other.getKey().manhattan(pos.getKey());
                if (manhattan > limit) {
                    continue;
                }
                if (other.getValue() - pos.getValue() - manhattan >= 100) {
                    ++count;
                }
            }
        }
        return count;
    }
}
