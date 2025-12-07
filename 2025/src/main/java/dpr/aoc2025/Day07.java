package dpr.aoc2025;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Point2D;
import dpr.commons.Util;

class Day07 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 7;
    }

    int part1(List<String> lines) {
        var maxY = lines.size();
        Pair<Point2D, Set<Point2D>> input = readPoints(lines);
        Point2D start = input.first();
        var splitters = input.second();
        var visitedSplitters = new HashSet<Point2D>();
        var visitedPoints = new HashSet<Point2D>();
        var streams = new Stack<Point2D>();
        streams.push(start);
        while (!streams.isEmpty()) {
            var cur = streams.pop();
            if (visitedPoints.contains(cur)) {
                continue;
            }
            visitedPoints.add(cur);
            while (cur.getY() <= maxY) {
                cur = cur.down(1);
                if (splitters.contains(cur)) {
                    visitedSplitters.add(cur);
                    streams.push(cur.left(1));
                    streams.push(cur.right(1));
                    break;
                }
                if (visitedSplitters.contains(cur)) {
                    break;
                }
            }

        }
        return visitedSplitters.size();
    }

    long part2(List<String> lines) {
        var maxY = lines.size();
        Pair<Point2D, Set<Point2D>> input = readPoints(lines);
        Point2D start = input.first();
        var splitters = input.second();
        var timelinesCache = new HashMap<Point2D, Long>();
        splitters.stream().sorted((a, b) -> b.getY() - a.getY())
                .forEach(cur -> {
                    timelinesCache.put(cur, analyze(cur.left(1), timelinesCache, maxY) + analyze(cur.right(1), timelinesCache, maxY));
                });
        return analyze(start, timelinesCache, maxY);
    }

    private static Pair<Point2D, Set<Point2D>> readPoints(List<String> lines) {
        var splitters = new HashSet<Point2D>();
        Point2D start = null;
        for (int y = 0; y < lines.size(); y++) {
            for (int x = 0; x < lines.get(y).length(); x++) {
                if (lines.get(y).charAt(x) == 'S') {
                    start = new Point2D(x, y);
                } else if (lines.get(y).charAt(x) == '^') {
                    splitters.add(new Point2D(x, y));
                }
            }
        }
        return new Pair<>(start, splitters);
    }

    private long analyze(Point2D cur, HashMap<Point2D, Long> timelinesCache, int maxY) {
        while (true) {
            cur = cur.down(1);
            if (cur.getY() > maxY) {
                return 1;
            } else if (timelinesCache.containsKey(cur)) {
                return timelinesCache.get(cur);
            }
        }
    }


    public static void main(String[] args) {
        new Day07().execute();
    }
}
