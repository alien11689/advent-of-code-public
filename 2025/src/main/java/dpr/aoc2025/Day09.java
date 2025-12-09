package dpr.aoc2025;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;

import org.jetbrains.annotations.NotNull;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Point2D;
import dpr.commons.Util;

class Day09 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 9;
    }

    long part1(List<String> lines) {
        var points = readPoints(lines);
        var bestArea = 0L;
        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                var area = calculateArea(points.get(i), points.get(j));
                if (area > bestArea) {
                    bestArea = area;
                }
            }
        }
        return bestArea;
    }

    private static List<Point2D> readPoints(List<String> lines) {
        return lines.stream().map(l -> {
            String[] parts = l.split(",");
            return new Point2D(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
        }).toList();
    }

    private long calculateArea(Point2D a, Point2D b) {
        return (1L + Math.abs(b.getX() - a.getX())) * (1L + Math.abs(b.getY() - a.getY()));
    }

    long part2(List<String> lines) {
        var points = readPoints(lines);
        var rules = detectLines(points);
        var bestArea = 0L;
        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                Point2D a = points.get(i);
                Point2D b = points.get(j);
                if (a.getX() == b.getX() || a.getY() == b.getY()) {
                    continue;
                }
                var area = calculateArea(a, b);
                if (area > bestArea) {
                    int minX = Math.min(a.getX(), b.getX());
                    int maxX = Math.max(a.getX(), b.getX());
                    int minY = Math.min(a.getY(), b.getY());
                    int maxY = Math.max(a.getY(), b.getY());
                    var xRange = new Pair<>(minX, maxX);
                    var yRange = new Pair<>(minY, maxY);
                    if (rules.stream().noneMatch(r -> r.intercepts(xRange, yRange))) {
                        // Perfect solution would detect rectangles outside the shape but it is not necessary
                        bestArea = area;
                    }
                }
            }
        }
        return bestArea;
    }

    @NotNull
    private List<Line> detectLines(List<Point2D> points) {
        var prevPoint = points.stream().min(Comparator.comparing(Point2D::getY).thenComparing(Point2D::getX)).get();
        var i = (points.indexOf(prevPoint) + 1) % points.size();
        var visited = new HashSet<Integer>();
        var lines = new ArrayList<Line>();
        var constant = Const.Y;
        while (!visited.contains(i)) {
            var curPoint = points.get(i);
            visited.add(i);

            var line = new Line(constant, constant == Const.Y ? prevPoint.getY() : prevPoint.getX(), range(constant, prevPoint, curPoint));
            lines.add(line);

            i = (i + 1) % points.size();

            constant = constant.opposite();
            prevPoint = curPoint;
        }
        return lines;
    }

    private Pair<Integer, Integer> range(Const target, Point2D point, Point2D nextPoint) {
        if (target == Const.Y) {
            var x1 = point.getX();
            var x2 = nextPoint.getX();
            var minX = Math.min(x1, x2);
            var maxX = Math.max(x1, x2);
            return new Pair<>(minX, maxX);
        }
        var y1 = point.getY();
        var y2 = nextPoint.getY();
        var minY = Math.min(y1, y2);
        var maxY = Math.max(y1, y2);
        return new Pair<>(minY, maxY);
    }

    enum Const {
        X, Y;

        public Const opposite() {
            return this == X ? Y : X;
        }
    }

    record Line(Const constant, int value, Pair<Integer, Integer> range) {
        public boolean intercepts(Pair<Integer, Integer> xRange, Pair<Integer, Integer> yRange) {
            if (constant == Const.X) { // vertical line
                if (value > xRange.first() && value < xRange.second() && overlap(range, yRange)) {
                    return true;
                }
            } else { // horizontal line
                if (value > yRange.first() && value < yRange.second() && overlap(range, xRange)) {
                    return true;
                }
            }
            return false;
        }

        private boolean overlap(Pair<Integer, Integer> range, Pair<Integer, Integer> other) {
            int a1 = range.first();
            int b1 = range.second();
            int a2 = other.first();
            int b2 = other.second();
            if (a1 == a2 || b1 == b2) {
                return true;
            }
            if (a1 < a2) {
                return b1 > a2;
            } else {
                return b2 > a1;
            }
        }
    }


    public static void main(String[] args) {
        new Day09().execute();
    }
}
