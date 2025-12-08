package dpr.aoc2025;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jetbrains.annotations.NotNull;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Point3D;
import dpr.commons.Util;

class Day08 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        int limit = 1000;
        Pair<Long, Long> solution = part1And2(lines, limit);
        System.out.println(solution.first());
        System.out.println(solution.second());
    }

    @Override
    public int dayNum() {
        return 8;
    }

    Pair<Long, Long> part1And2(List<String> lines, int limit) {
        var points = new ArrayList<Point3D>();
        for (String line : lines) {
            var parts = line.split(",");
            points.add(new Point3D(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]), Integer.parseInt(parts[2])));
        }
        var connections = new ArrayList<Connection>();
        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                var point1 = points.get(i);
                var point2 = points.get(j);
                List<Point3D> pair = List.of(point1, point2);
                connections.add(new Connection(pair, euclidean(point1, point2)));
            }
        }

        Collections.sort(connections);

        var connected = new HashSet<Set<Point3D>>();

        var connectionsMade = 0;
        var solutionPart1 = 0L;
        var solutionPart2 = 0L;
        for (var connection : connections) {
            var point1 = connection.points.get(0);
            var point2 = connection.points.get(1);
            var maybeSet1 = connected.stream().filter(s -> s.contains(point1)).findAny();
            var maybeSet2 = connected.stream().filter(s -> s.contains(point2)).findAny();
            if (maybeSet1.isPresent() && maybeSet2.isPresent()) {
                Set<Point3D> set1 = maybeSet1.get();
                Set<Point3D> set2 = maybeSet2.get();
                if (set1.equals(set2)) {
                    ++connectionsMade;
                    continue;
                }
                var del1 = connected.remove(set1);
                var del2 = connected.remove(set2);
                if (!del1 || !del2) {
                    throw new RuntimeException("Invalid remove from set");
                }
                var newSet = new HashSet<>(set1);
                newSet.addAll(set2);
                connected.add(newSet);
                ++connectionsMade;
            } else if (maybeSet1.isPresent() && maybeSet2.isEmpty()) {
                var newSet = new HashSet<>(maybeSet1.get());
                newSet.add(point2);
                connected.remove(maybeSet1.get());
                connected.add(newSet);
                ++connectionsMade;
            } else if (maybeSet1.isEmpty() && maybeSet2.isPresent()) {
                var newSet = new HashSet<>(maybeSet2.get());
                newSet.add(point1);
                connected.remove(maybeSet2.get());
                connected.add(newSet);
                ++connectionsMade;
            } else {
                connected.add(new HashSet<>(connection.points));
                ++connectionsMade;
            }

            if (connectionsMade == limit) {
                solutionPart1 = connected.stream().map(s -> s.size()).sorted((o1, o2) -> o2 - o1).limit(3).reduce(1, Math::multiplyExact);
            }
            if (connected.stream().findAny().get().size() == points.size()) {
                solutionPart2 = 1L * connection.points.get(0).getX() * connection.points.get(1).getX();
                break;
            }
        }

        return new Pair<>(solutionPart1, solutionPart2);
    }

    private double euclidean(Point3D point1, Point3D point2) {
        return Math.sqrt(
                Math.pow(point1.getX() - point2.getX(), 2) +
                        Math.pow(point1.getY() - point2.getY(), 2) +
                        Math.pow(point1.getZ() - point2.getZ(), 2)
        );
    }

    record Connection(List<Point3D> points, double distance) implements Comparable<Connection> {
        @Override
        public int compareTo(@NotNull Connection o) {
            return Double.compare(distance, o.distance);
        }
    }

    public static void main(String[] args) {
        new Day08().execute();
    }
}
