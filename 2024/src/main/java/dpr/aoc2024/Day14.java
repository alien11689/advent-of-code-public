package dpr.aoc2024;

import dpr.commons.Util;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

class Day14 implements Day {
    public static void main(String... args) {
        new Day14().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
            var X = 101;
            var Y = 103;
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var X = 11;
//            var Y = 7;
            System.out.println(part1(lines, X, Y));
            System.out.println(part2(lines));
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

    private Object part2(List<String> lines) {
        return null;
    }
}
