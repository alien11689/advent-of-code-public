package dpr.aoc2024;

import dpr.commons.Point2D;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

class Day12 implements Day {
    public static void main(String... args) {
        new Day12().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test2.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test3.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test4.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test5.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 12;
    }

    private Object part1(List<String> lines) {
        Map<Point2D, Character> map = readMap(lines);
        Map<Point2D, Character> mapDuplicate = new HashMap<>(map);
        long result = 0;
        while (!mapDuplicate.isEmpty()) {
            Map.Entry<Point2D, Character> root = mapDuplicate.entrySet().stream().findAny().get();
            Character type = root.getValue();
            Set<Point2D> points = findSingleRegion(root.getKey(), type, map);
            points.forEach(mapDuplicate::remove);
            long perimeter = points.stream().mapToLong(e -> e.neighboursCross().stream().filter(p -> !type.equals(map.get(p))).count()).sum();
            int area = points.size();
//            System.out.println("For " + type + " area is " + area + " and perimeter is " + perimeter);
            result += area * perimeter;
        }
        return result;
    }

    @NotNull
    private static Set<Point2D> findSingleRegion(Point2D rootPoint, Character type, Map<Point2D, Character> map) {
        Queue<Point2D> q = new LinkedList<>();
        q.offer(rootPoint);
        Set<Point2D> points = new HashSet<>();
        while (!q.isEmpty()) {
            Point2D cur = q.poll();
            if (points.contains(cur)) {
                continue;
            }
            points.add(cur);
            cur.neighboursCross().forEach(n -> {
                if (type.equals(map.get(n))) {
                    q.offer(n);
                }
            });
        }
        return points;
    }

    @NotNull
    private static Map<Point2D, Character> readMap(List<String> lines) {
        Map<Point2D, Character> map = new HashMap<>();
        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            for (int x = 0; x < line.length(); x++) {
                map.put(new Point2D(x, y), line.charAt(x));
            }
        }
        return map;
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
