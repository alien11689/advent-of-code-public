package dpr.aoc2024;

import dpr.commons.Dir;
import dpr.commons.Point2D;
import dpr.commons.Util;
import kotlin.Pair;

import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;

class Day16 implements Day {
    public static void main(String... args) {
        new Day16().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test2.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 16;
    }

    record Pos(Point2D p, Dir d, int points) {
        Pair<Point2D, Dir> key() {
            return new Pair<>(p, d);
        }
    }

    private Object part1(List<String> lines) {
        Point2D start = null;
        Point2D end = null;
        Set<Point2D> blocks = new HashSet<>();
        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            for (int x = 0; x < line.length(); x++) {
                char c = line.charAt(x);
                if (c == '#') {
                    blocks.add(new Point2D(x, y));
                } else if (c == 'S') {
                    start = new Point2D(x, y);
                } else if (c == 'E') {
                    end = new Point2D(x, y);
                }
            }
        }

        Point2D finalEnd = end;
        PriorityQueue<Pos> q = new PriorityQueue<>(new Comparator<Pos>() {
            @Override
            public int compare(Pos o1, Pos o2) {
                int manhattan1 = o1.p.manhattan(finalEnd);
                int manhattan2 = o2.p.manhattan(finalEnd);
                if (manhattan1 == manhattan2) {
                    return Integer.compare(o1.points, o2.points);
                }
                return Integer.compare(manhattan1, manhattan2);
            }
        });

        Map<Pair<Point2D, Dir>, Integer> memory = new HashMap<>();

        q.offer(new Pos(start, Dir.E, 0));
        q.offer(new Pos(start, Dir.N, 1000));
        q.offer(new Pos(start, Dir.S, 1000));
        q.offer(new Pos(start, Dir.W, 2000));

        Integer bestScore = Integer.MAX_VALUE;
        while (!q.isEmpty()) {
//            System.out.println("Q size is " + q.size());
            Pos cur = q.poll();
            Pair<Point2D, Dir> key = cur.key();
            if (memory.getOrDefault(key, Integer.MAX_VALUE) < cur.points) {
                continue;
            }
            if (cur.points >= bestScore) {
                continue;
            }
            memory.put(key, cur.points);
            Point2D point = cur.p;
            int newScore = cur.points;
            while (true) {
                Point2D np = point.move(cur.d, 1);
//                System.out.println("Checking " + np);
                if (blocks.contains(np)) {
//                    if (newScore != cur.points) {
//                        q.offer(new Pos(np, cur.d, newScore));
//                    }
                    break;
                }
                ++newScore;
                if (np.equals(end)) {
                    if (newScore < bestScore) {
                        System.out.println("Found better score: " + newScore);
                        bestScore = newScore;
                    }
                    break;
                }
                Dir left = cur.d.turnLeft();
                Dir right = cur.d.turnRight();
                if (!blocks.contains(np.move(left, 1))) {
                    q.offer(new Pos(np, left, newScore + 1000));
                }
                if (!blocks.contains(np.move(right, 1))) {
                    q.offer(new Pos(np, right, newScore + 1000));
                }
                point = np;
            }
        }
        return bestScore;
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
