package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Point2D;
import dpr.commons.Util;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.stream.Collectors;

class Day14 implements Day {
    public static void main(String... args) {
        new Day14().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
            var X = 101;
            var Y = 103;
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var X = 11;
//            var Y = 7;
            System.out.println(part1(lines, X, Y));
            System.out.println(part2(lines, X, Y));
        });
    }

    @Override
    public int dayNum() {
        return 14;
    }

    record Robot(BigInteger x, BigInteger y, BigInteger dx, BigInteger dy) {

        public Robot iterate(int iter, int X, int Y) {
            BigInteger xLimit = BigInteger.valueOf(X);
            BigInteger iterLimit = BigInteger.valueOf(iter);
            BigInteger yLimit = BigInteger.valueOf(Y);
            var newX = x.add(dx.multiply(iterLimit).mod(xLimit)).add(xLimit).mod(xLimit);
            var newY = y.add(dy.multiply(iterLimit).mod(yLimit)).add(yLimit).mod(yLimit);
            return new Robot(newX, newY, dx, dy);
        }

        public boolean inQuadrant(int minX, int maxX, int minY, int maxY) {
            int xx = x.intValue();
            int yy = y.intValue();

            return xx >= minX && xx <= maxX && yy >= minY && yy <= maxY;
        }
    }

    private Object part1(List<String> lines, int X, int Y) {
        List<Robot> robots = new ArrayList<>();
        lines.forEach(line -> {
            String[] parts = line.split("[=, ]");
            Robot robot = new Robot(new BigInteger(parts[1]), new BigInteger(parts[2]), new BigInteger(parts[4]), new BigInteger(parts[5]));
//            System.out.println(robot);
            Robot afterIterations = robot.iterate(100, X, Y);
            robots.add(afterIterations);
//            System.out.println(afterIterations);
        });
        int halfX = X / 2;
        int halfY = Y / 2;
        long q1 = robots.stream().filter(r -> r.inQuadrant(0, halfX - 1, 0, halfY - 1)).count();
        long q2 = robots.stream().filter(r -> r.inQuadrant(halfX + 1, X, 0, halfY - 1)).count();
        long q3 = robots.stream().filter(r -> r.inQuadrant(0, halfX - 1, halfY + 1, Y)).count();
        long q4 = robots.stream().filter(r -> r.inQuadrant(halfX + 1, X, halfY + 1, Y)).count();
        return q1 * q2 * q3 * q4;
    }

    record Robot2(int x, int y, int dx, int dy) {
        public Robot2 iterate(int X, int Y) {
            return new Robot2(((x + dx) % X + X) % X, ((y + dy) % Y + Y) % Y, dx, dy);
        }
    }

    private Object part2(List<String> lines, int X, int Y) {
        Set<Robot2> initialRobots = new HashSet<>();
        lines.forEach(line -> {
            String[] parts = line.split("[=, ]");
            Robot2 robot = new Robot2(Integer.parseInt(parts[1]), Integer.parseInt(parts[2]), Integer.parseInt(parts[4]), Integer.parseInt(parts[5]));
            initialRobots.add(robot);
        });
        Set<Robot2> robots = initialRobots;
        long count = 0;
        while (true) {
            ++count;
//            System.out.println(count);
            robots = robots.stream().map(r -> r.iterate(X, Y)).collect(Collectors.toSet());
            Set<Point2D> points = robots.stream().map(r -> new Point2D(r.x, r.y)).collect(Collectors.toSet());
            int max = clusters(points).stream().mapToInt(Set::size).max().getAsInt();
            if (max >= points.size() / 4) {
//                for (int y = 0; y < Y; ++y) {
//                    for (int x = 0; x < X; ++x) {
//                        System.out.print(points.contains(new Point2D(x, y)) ? '#' : '.');
//                    }
//                    System.out.println();
//                }
                return count;
            }
        }
    }

    private Set<Set<Point2D>> clusters(Set<Point2D> all) {
        Set<Set<Point2D>> result = new HashSet<>();
        Set<Point2D> copy = new HashSet<>(all);
        while (!copy.isEmpty()) {
            Point2D root = copy.stream().findAny().get();
            Queue<Point2D> q = new LinkedList<>();
            q.offer(root);
            Set<Point2D> visited = new HashSet<>();
            while (!q.isEmpty()) {
                Point2D cur = q.poll();
                if (visited.contains(cur)) {
                    continue;
                }
                visited.add(cur);
                cur.neighbours().forEach(n -> {
                    if (copy.contains(n) && !visited.contains(n)) {
                        q.offer(n);
                    }
                });
            }
            copy.removeAll(visited);
            result.add(visited);
        }
        return result;
    }
}
