package dpr.aoc2024;

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
import java.util.stream.Collectors;

class Day18 implements Day {
    public static void main(String... args) {
        new Day18().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
            int max = 70;
            int take = 1024;
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            int max = 6;
//            int take = 12;
            Pair<Object, Object> solution1And2 = part1And2(lines, max, take);
            System.out.println(solution1And2.getFirst());
            System.out.println(solution1And2.getSecond());
        });
    }

    @Override
    public int dayNum() {
        return 18;
    }

    record Position(Point2D p, int steps, int fitness, Position prev) {
    }

    private Pair<Object, Object> part1And2(List<String> lines, int max, int take) {
        Point2D start = new Point2D(0, 0);
        Point2D target = new Point2D(max, max);
        Set<Point2D> blocks = lines.stream().limit(take).map(line -> {
            String[] parts = line.split(",");
            return new Point2D(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
        }).collect(Collectors.toSet());

//        for (int y = 0; y <= max; ++y) {
//            for (int x = 0; x <= max; x++) {
//                System.out.print(blocks.contains(new Point2D(x, y)) ? '#' : '.');
//            }
//            System.out.println();
//        }
        Position currentPath = iterate(max, start, target, blocks);
        int part1 = currentPath.steps;

        Set<Point2D> bestPath = new HashSet<>();
        while (currentPath != null) {
            bestPath.add(currentPath.p);
            currentPath = currentPath.prev;
        }

        String part2 = "";
        List<Point2D> newBlocks = lines.stream().skip(take).map(line -> {
            String[] parts = line.split(",");
            return new Point2D(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
        }).toList();
        for (Point2D newBlock : newBlocks) {
//            System.out.println("Checking " + newBlock);
            blocks.add(newBlock);
            if (!bestPath.contains(newBlock)) {
                continue;
            }
            Position result = iterate(max, start, target, blocks);
            if (result == null) {
                part2 = newBlock.getX() + "," + newBlock.getY();
                break;
            }
            bestPath = new HashSet<>();
            while (result != null) {
                bestPath.add(result.p);
                result = result.prev;
            }
        }
        return new Pair<>(part1, part2);
    }

    private static Position iterate(int max, Point2D start, Point2D target, Set<Point2D> blocks) {
        Map<Point2D, Integer> memory = new HashMap<>();
        PriorityQueue<Position> pq = new PriorityQueue<>(Comparator.comparingInt((Position o) -> o.fitness).thenComparingInt(o -> o.steps));
        pq.offer(new Position(start, 0, start.manhattan(target), null));
        int minimal = Integer.MAX_VALUE;
        Position best = null;
        while (!pq.isEmpty()) {
            Position cur = pq.poll();
//            System.out.println("Pq size is " + pq.size() + " mem size is " + memory.size() + " cur " + cur);
            if (memory.getOrDefault(cur.p, Integer.MAX_VALUE) <= cur.steps) {
                continue;
            }
            memory.put(cur.p, cur.steps);
            if (cur.steps >= minimal) {
                continue;
            }
            if (cur.p.equals(target)) {
//                System.out.println("Setting minimal to " + cur.steps);
                minimal = cur.steps;
                best = cur;
                continue;
            }
            cur.p.neighboursCross()
                    .stream()
                    .filter(p -> !blocks.contains(p) && p.inRange(0, max))
                    .filter(p -> memory.getOrDefault(p, Integer.MAX_VALUE) > cur.steps + 1)
                    .forEach(next -> pq.offer(new Position(next, cur.steps + 1, next.manhattan(target), cur))
                    );
        }
        return best;
    }
}
