package dpr.aoc2025;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jetbrains.annotations.NotNull;

import dpr.commons.Day;
import dpr.commons.Point2D;
import dpr.commons.Util;

public class Day04 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 4;
    }

    int part1(List<String> lines) {
        var points = readBoard(lines);
        return getRollsToRemove(points).size();
    }

    int part2(List<String> lines) {
        var points = readBoard(lines);
        int result = 0;
        while (true) {
            var toRemove = getRollsToRemove(points);
            result += toRemove.size();
            points.removeAll(toRemove);
            if (toRemove.isEmpty()) {
                break;
            }
        }
        return result;
    }

    @NotNull
    private static Set<Point2D> getRollsToRemove(Set<Point2D> points) {
        Set<Point2D> toRemove = new HashSet<>();
        for (Point2D cur : points) {
            long count = cur.adjacentPoints().stream().filter(points::contains).count();
            if (count < 4) {
                toRemove.add(cur);
            }
        }
        return toRemove;
    }

    @NotNull
    private static Set<Point2D> readBoard(List<String> lines) {
        Set<Point2D> points = new HashSet<>();
        for (int i = 0; i < lines.size(); i++) {
            for (int j = 0; j < lines.get(i).length(); j++) {
                if (lines.get(i).charAt(j) == '@') {
                    points.add(new Point2D(i, j));
                }
            }
        }
        return points;
    }


    public static void main(String[] args) {
        new Day04().execute();
    }
}
