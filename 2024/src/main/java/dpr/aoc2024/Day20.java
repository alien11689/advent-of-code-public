package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Point2D;
import dpr.commons.Util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

class Day20 implements Day {
    public static void main(String... args) {
        new Day20().execute();
    }

    @Override
    public void execute() {
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
        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            for (int x = 0; x < line.length(); x++) {
                Point2D p = new Point2D(x, y);
                char c = line.charAt(x);
                if (c == '#') {
                    blocks.add(p);
                } else if (c == 'S') {
                    start = p;
                }
            }
        }

        List<Point2D> path = new ArrayList<>();
        Set<Point2D> visited = new HashSet<>();
        Queue<Point2D> q = new LinkedList<>();
        q.offer(start);
        while (!q.isEmpty()) {
            if (q.size() > 1) {
                throw new RuntimeException();
            }
            Point2D cur = q.poll();
            visited.add(cur);
            path.add(cur);
            List<Point2D> neighbours = cur.neighboursCross();
            for (Point2D n : neighbours) {
                if (!blocks.contains(n) && !visited.contains(n)) {
                    q.offer(n);
                }
            }
        }

        long count = 0;
        for (int i = 0; i < path.size(); i++) {
            Point2D pos = path.get(i);
            for (int j = i + 1; j < path.size(); j++) {
                Point2D other = path.get(j);
                int manhattan = other.manhattan(pos);
                if (manhattan > limit) {
                    continue;
                }
                if (j - i - manhattan >= 100) {
                    ++count;
                }
            }
        }
        return count;
    }
}
