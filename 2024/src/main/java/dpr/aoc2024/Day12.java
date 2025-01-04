package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Dir;
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
import java.util.stream.Collectors;

class Day12 implements Day {
    public static void main(String... args) {
        new Day12().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 12;
    }

    Object part1(List<String> lines) {
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

    Object part2(List<String> lines) {
        Map<Point2D, Character> map = readMap(lines);
        Map<Point2D, Character> mapDuplicate = new HashMap<>(map);
        long result = 0;
        while (!mapDuplicate.isEmpty()) {
            Map.Entry<Point2D, Character> root = mapDuplicate.entrySet().stream().findAny().get();
            Character type = root.getValue();
            Set<Point2D> points = findSingleRegion(root.getKey(), type, map);
            points.forEach(mapDuplicate::remove);
            long area = points.size();
            long sides = 0;
            for (Dir d : Dir.getEntries()) {
                Set<Point2D> border = points.stream().map(p -> p.move(d, 1)).filter(p -> !points.contains(p)).collect(Collectors.toSet());
                sides += clustersCross(border).size();
            }
//            System.out.println("For " + type + " area is " + area + " and sides are " + sides);
            result += area * sides;
        }
        return result;
    }

    private Set<Set<Point2D>> clustersCross(Set<Point2D> borderUp) {
        Set<Set<Point2D>> result = new HashSet<>();
        Set<Point2D> copy = new HashSet<>(borderUp);
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
                cur.neighboursCross().forEach(n -> {
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
