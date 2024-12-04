package dpr.aoc2024;

import dpr.commons.Point2D;
import dpr.commons.Util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

class Day04 implements Day {
    public static void main(String... args) {
        new Day04().execute(args);
    }

    private static final Character x = 'X';
    private static final Character m = 'M';
    private static final Character a = 'A';
    private static final Character s = 'S';

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 4;
    }

    private Object part1(List<String> lines) {
        var map = new HashMap<Point2D, Character>();
        for (int y = 0; y < lines.size(); y++) {
            var line = lines.get(y).toCharArray();
            for (int x = 0; x < line.length; x++) {
                map.put(new Point2D(x, y), line[x]);
            }
        }
        return map.entrySet()
                .stream()
                .filter(e -> x.equals(e.getValue()))
                .map(e -> e.getKey())
                .flatMap(p -> continueAfterX(p, map).stream())
                .count();
    }

    private List<Set<Point2D>> continueAfterX(Point2D p, Map<Point2D, Character> map) {
        List<Set<Point2D>> xmas = new ArrayList<>();
        for (Point2D possibleM : p.adjacentPoints()){
            if (m.equals(map.get(possibleM))){
               int dx = possibleM.getX() - p.getX();
               int dy = possibleM.getY() - p.getY();
                Point2D possibleA = possibleM.move(dx, dy);
                Point2D possibleS = possibleA.move(dx, dy);
                if(a.equals(map.get(possibleA)) && s.equals(map.get(possibleS))){
                    xmas.add(Set.of(p, possibleM, possibleA, possibleS));
                }
            }
        }
        return xmas;
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
