package dpr.aoc2024;

import dpr.commons.Point2D;
import dpr.commons.Util;
import kotlin.Pair;

import java.util.Comparator;
import java.util.HashMap;
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
            part1And2(lines, max, take);
        });
    }

    @Override
    public int dayNum() {
        return 18;
    }

    record Position(Point2D p, int steps, int fitness) {
    }

    private void part1And2(List<String> lines, int max, int take) {
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
        int part1 = iterate(max, start, target, blocks);
        System.out.println(part1);
        String part2 = lines.stream().skip(take).map(line -> {
                    String[] parts = line.split(",");
                    return new Point2D(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
                }).map(newBlock -> {
                            blocks.add(newBlock);
                            int minimal = iterate(max, start, target, blocks);
                            return new Pair<>(newBlock, minimal);
                        }
                ).filter(p -> p.getSecond() == Integer.MAX_VALUE)
                .findFirst()
                .map(p -> p.getFirst().getX() + "," + p.getFirst().getY())
                .get();
        System.out.println(part2);
    }

    private static int iterate(int max, Point2D start, Point2D target, Set<Point2D> blocks) {
        Map<Point2D, Integer> memory = new HashMap<>();
        PriorityQueue<Position> pq = new PriorityQueue<>(new Comparator<Position>() {
            @Override
            public int compare(Position o1, Position o2) {
                int fitness = Integer.compare(o1.fitness, o2.fitness);
                if (fitness != 0) {
                    return fitness;
                }
                return Integer.compare(o1.steps, o2.steps);
            }
        });
        pq.offer(new Position(start, 0, start.manhattan(target)));
        int minimal = Integer.MAX_VALUE;
        while (!pq.isEmpty()) {
            Position cur = pq.poll();
//            System.out.println("Pq size is " + pq.size() + " mem size is " + memory.size() + " cur " + cur);
            if (memory.getOrDefault(cur.p, Integer.MAX_VALUE) < cur.steps) {
                continue;
            }
            memory.put(cur.p, cur.steps);
            if (cur.steps >= minimal) {
                continue;
            }
            if (cur.p.equals(target)) {
//                System.out.println("Setting minimal to " + cur.steps);
                minimal = cur.steps;
                continue;
            }
            cur.p.neighboursCross()
                    .stream()
                    .filter(p -> !blocks.contains(p) && p.inRange(0, max))
                    .filter(p -> memory.getOrDefault(p, Integer.MAX_VALUE) > cur.steps + 1)
                    .forEach(next ->
                            pq.offer(new Position(next, cur.steps + 1, next.manhattan(target))));
        }
        return minimal;
    }
}
