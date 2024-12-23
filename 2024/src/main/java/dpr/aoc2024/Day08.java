package dpr.aoc2024;

import dpr.commons.Point2D;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

class Day08 implements Day {
    public static void main(String... args) {
        new Day08().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 8;
    }

    private Object part1(List<String> lines) {
        Map<Character, List<Point2D>> namedAntenas = readNamedAntenas(lines);
//        System.out.println(namedAntenas);
        Set<Point2D> antinodes = new HashSet<>();
        for (Map.Entry<Character, List<Point2D>> entry : namedAntenas.entrySet()) {
            List<Point2D> antenas = entry.getValue();
//            System.out.println("Checking "+ entry.getKey());
            for (int i = 0; i < antenas.size() - 1; i++) {
                for (int j = i + 1; j < antenas.size(); j++) {
                    Point2D first = antenas.get(i);
                    Point2D second = antenas.get(j);
                    int dx = second.getX() - first.getX();
                    int dy = second.getY() - first.getY();
                    Point2D antinode1 = new Point2D(first.getX() - dx, first.getY() - dy);
                    Point2D antinode2 = new Point2D(second.getX() + dx, second.getY() + dy);
                    antinodes.add(antinode1);
                    antinodes.add(antinode2);
//                    System.out.println(first + " and " + second + " has antinodes " + antinode1 + " and " + antinode2);
                }
            }
        }
        return antinodes.stream()
                .filter(p -> inBound(lines, p))
                .count();
    }

    @NotNull
    private static Map<Character, List<Point2D>> readNamedAntenas(List<String> lines) {
        Map<Character, List<Point2D>> namedAntenas = new HashMap<>();
        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            for (int x = 0; x < line.length(); x++) {
                char c = line.charAt(x);
                if (c != '.') {
                    namedAntenas.computeIfAbsent(c, k -> new ArrayList<>()).add(new Point2D(x, y));
                }
            }
        }
        return namedAntenas;
    }

    private Object part2(List<String> lines) {
        Map<Character, List<Point2D>> namedAntenas = readNamedAntenas(lines);
//        System.out.println(namedAntenas);
        Set<Point2D> antinodes = new HashSet<>();
        for (Map.Entry<Character, List<Point2D>> entry : namedAntenas.entrySet()) {
            List<Point2D> antenas = entry.getValue();
//            System.out.println("Checking "+ entry.getKey());
            for (int i = 0; i < antenas.size() - 1; i++) {
                for (int j = i + 1; j < antenas.size(); j++) {
                    Point2D first = antenas.get(i);
                    Point2D second = antenas.get(j);
                    int dx = second.getX() - first.getX();
                    int dy = second.getY() - first.getY();
                    Point2D firstMul = first;
                    while (true) {
                        firstMul = new Point2D(firstMul.getX() - dx, firstMul.getY() - dy);
                        if (inBound(lines, firstMul)) {
                            antinodes.add(firstMul);
                        } else {
                            break;
                        }
                    }

                    Point2D secondMul = second;
                    while (true) {
                        secondMul = new Point2D(secondMul.getX() + dx, secondMul.getY() + dy);
                        if (inBound(lines, secondMul)) {
                            antinodes.add(secondMul);
                        } else {
                            break;
                        }
                    }
//                    System.out.println(first + " and " + second + " has antinodes " + antinode1 + " and " + antinode2);
                }
            }
            antinodes.addAll(antenas);
        }
        return antinodes.size();
    }

    private static boolean inBound(List<String> lines, Point2D p) {
        return p.getX() >= 0 && p.getY() >= 0 && p.getX() < lines.get(0).length() && p.getY() < lines.size();
    }
}
