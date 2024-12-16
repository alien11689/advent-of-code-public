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
            part1And2(lines);
        });
    }

    @Override
    public int dayNum() {
        return 16;
    }

    private void draw(Set<Point2D> blocks, Set<Point2D> bestPaths) {
        List<Point2D> list = blocks.stream().sorted().toList();
        Point2D last = list.getLast();
        for (int y = 0; y <= last.getY(); ++y) {
            for (int x = 0; x <= last.getX(); x++) {
                Point2D cur = new Point2D(x, y);
                System.out.print(blocks.contains(cur) ? '#' : bestPaths.contains(cur) ? 'O' : '.');
            }
            System.out.println();
        }
    }

    record Position(Point2D p, Dir d, int points, Set<Point2D> visited) {
        Pair<Point2D, Dir> key() {
            return new Pair<>(p, d);
        }
    }

    private void part1And2(List<String> lines) {
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
        PriorityQueue<Position> q = new PriorityQueue<>(new Comparator<Position>() {
            @Override
            public int compare(Position o1, Position o2) {
                int manhattan1 = o1.p.manhattan(finalEnd);
                int manhattan2 = o2.p.manhattan(finalEnd);
                if (manhattan1 == manhattan2) {
                    return Integer.compare(o1.points, o2.points);
                }
                return Integer.compare(manhattan1, manhattan2);
            }
        });

        Map<Pair<Point2D, Dir>, Integer> memory = new HashMap<>();

        q.offer(new Position(start, Dir.E, 0, Set.of(start)));
        q.offer(new Position(start, Dir.N, 1000, Set.of(start)));
        q.offer(new Position(start, Dir.S, 1000, Set.of(start)));
        q.offer(new Position(start, Dir.W, 2000, Set.of(start)));

        Integer bestScore = Integer.MAX_VALUE;
        Set<Point2D> bestPaths = new HashSet<>();
        while (!q.isEmpty()) {
//            System.out.println("Q size is " + q.size());
            Position cur = q.poll();
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
            Set<Point2D> localVisited = new HashSet<>();
            while (true) {
                Point2D np = point.move(cur.d, 1);
//                System.out.println("Checking " + np);
                if (blocks.contains(np)) {
                    break;
                }
                ++newScore;
                localVisited.add(np);
                if (np.equals(end)) {
                    if (newScore < bestScore) {
//                        System.out.println("Found better score: " + newScore);
                        bestScore = newScore;
                        Set<Point2D> newBestPaths = new HashSet<>(cur.visited);
                        newBestPaths.addAll(localVisited);
                        bestPaths = newBestPaths;
//                        System.out.println("Local visited size is " + localVisited.size());
//                        System.out.println("Best size is " + bestPaths.size());
                    } else if (newScore == bestScore) {
//                        System.out.println("Found the same score: " + newScore);
                        bestPaths.addAll(cur.visited);
                        bestPaths.addAll(localVisited);
//                        System.out.println("Best size is " + bestPaths.size());
//                        System.out.println("Local visited size is " + localVisited.size());
                    }
                    break;
                }
                Dir left = cur.d.turnLeft();
                Dir right = cur.d.turnRight();
                Set<Point2D> bestPathsForSplit = new HashSet<>(cur.visited);
                bestPathsForSplit.addAll(localVisited);
                if (!blocks.contains(np.move(left, 1))) {
                    q.offer(new Position(np, left, newScore + 1000, bestPathsForSplit));
                }
                if (!blocks.contains(np.move(right, 1))) {
                    q.offer(new Position(np, right, newScore + 1000, bestPathsForSplit));
                }
                point = np;
            }
        }
//        draw(blocks, bestPaths);
        System.out.println(bestScore); // part 1
        System.out.println(bestPaths.size()); // part 2
    }
}
