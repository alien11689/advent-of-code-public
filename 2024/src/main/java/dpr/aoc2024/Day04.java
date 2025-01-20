package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Point2D;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

class Day04 implements Day {
    private static final Character x = 'X';
    private static final Character m = 'M';
    private static final Character a = 'A';
    private static final Character s = 'S';

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

    Object part1(List<String> lines) {
        var map = readMap(lines);
        return map.entrySet()
                .stream()
                .filter(e -> x.equals(e.getValue()))
                .map(Map.Entry::getKey)
                .mapToLong(p -> continueAfterX(p, map).size())
                .sum();
    }

    @NotNull
    private static Map<Point2D, Character> readMap(List<String> lines) {
        var map = new HashMap<Point2D, Character>();
        for (int y = 0; y < lines.size(); y++) {
            var line = lines.get(y).toCharArray();
            for (int x = 0; x < line.length; x++) {
                map.put(new Point2D(x, y), line[x]);
            }
        }
        return map;
    }

    private List<Set<Point2D>> continueAfterX(Point2D p, Map<Point2D, Character> map) {
        List<Set<Point2D>> xmas = new ArrayList<>();
        for (Point2D possibleM : p.adjacentPoints()) {
            if (m.equals(map.get(possibleM))) {
                int dx = possibleM.getX() - p.getX();
                int dy = possibleM.getY() - p.getY();
                Point2D possibleA = possibleM.move(dx, dy);
                Point2D possibleS = possibleA.move(dx, dy);
                if (a.equals(map.get(possibleA)) && s.equals(map.get(possibleS))) {
                    xmas.add(Set.of(p, possibleM, possibleA, possibleS));
                }
            }
        }
        return xmas;
    }

    Object part2(List<String> lines) {
        var map = readMap(lines);
        var count = 0;
        for (int y = 0; y < lines.size() - 2; ++y) {
            for (int x = 0; x < lines.get(y).length() - 2; ++x) {
                if (a.equals(map.get(new Point2D(x + 1, y + 1)))) {
                    if (m.equals(map.get(new Point2D(x, y))) && m.equals(map.get(new Point2D(x + 2, y))) && s.equals(map.get(new Point2D(x, y + 2))) && s.equals(map.get(new Point2D(x + 2, y + 2)))) {
                        ++count;
                    }
                    if (m.equals(map.get(new Point2D(x, y))) && m.equals(map.get(new Point2D(x, y + 2))) && s.equals(map.get(new Point2D(x + 2, y))) && s.equals(map.get(new Point2D(x + 2, y + 2)))) {
                        ++count;
                    }
                    if (s.equals(map.get(new Point2D(x, y))) && s.equals(map.get(new Point2D(x + 2, y))) && m.equals(map.get(new Point2D(x, y + 2))) && m.equals(map.get(new Point2D(x + 2, y + 2)))) {
                        ++count;
                    }
                    if (s.equals(map.get(new Point2D(x, y))) && s.equals(map.get(new Point2D(x, y + 2))) && m.equals(map.get(new Point2D(x + 2, y))) && m.equals(map.get(new Point2D(x + 2, y + 2)))) {
                        ++count;
                    }
                }
            }
        }
        return count;
    }
}
