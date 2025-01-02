package dpr.aoc2024;

import dpr.commons.Dir;
import dpr.commons.Pair;
import dpr.commons.Point2D;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

class Day06 implements Day {
    public static void main(String... args) {
        new Day06().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
            Pair<Object, Object> solution1And2 = part1And2(lines);
            System.out.println(solution1And2.first());
            System.out.println(solution1And2.second());
        });
    }

    @Override
    public int dayNum() {
        return 6;
    }

    record Position(Point2D point, Dir dir) {
    }

    private Pair<Object, Object> part1And2(List<String> lines) {
        Point2D current = null;
        Set<Point2D> blocks = new HashSet<>();
        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            for (int x = 0; x < line.length(); x++) {
                if (line.charAt(x) == '#') {
                    blocks.add(new Point2D(x, y));
                } else if (line.charAt(x) == '^') {
                    current = new Point2D(x, y);
                }
            }
        }
        Position starting = new Position(current, Dir.N);
        Set<Point2D> visited = travel(lines, current, blocks);
        int part1 = visited.size();
        Set<Point2D> possibleObstacle = visited.stream()
                .filter(p -> !p.equals(starting.point))
                .collect(Collectors.toSet());
        long count = possibleObstacle.stream()
                .parallel()
                .filter(obstacle -> {
                    Set<Point2D> newBlocks = new HashSet<>(blocks);
                    newBlocks.add(obstacle);
                    Set<Point2D> newVisited = travel(lines, starting.point, newBlocks);
                    return newVisited.isEmpty();
                })
                .count();
        return new Pair<>(part1, count);
    }

    @NotNull
    private static Set<Point2D> travel(List<String> lines, Point2D current, Set<Point2D> blocks) {
        Dir dir = Dir.N;
        Set<Point2D> visited = new HashSet<>();
        visited.add(current);
        Set<Position> memory = new HashSet<>();
        int xLimit = lines.get(1).length();
        int yLimit = lines.size();
        while (true) {
            Position p = new Position(current, dir);
            if (memory.contains(p)) {
                return Collections.emptySet();
            }
            memory.add(p);
            Point2D next = current.move(dir, 1);
            if (blocks.contains(next)) {
                dir = dir.turnRight();
            } else {
                if (next.getX() < 0 || next.getY() < 0 || next.getX() >= xLimit || next.getY() >= yLimit) {
                    break;
                } else {
                    visited.add(next);
                    current = next;
                }
            }
        }
        return visited;
    }
}
