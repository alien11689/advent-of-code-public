package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Point2D;
import dpr.commons.Util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

class Day10 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        Pair<Object, Object> solution1And2 = part1And2(lines);
        System.out.println(solution1And2.first());
        System.out.println(solution1And2.second());
    }

    @Override
    public int dayNum() {
        return 10;
    }

    record Position(Point2D loc, int num, Point2D start) {
    }

    Pair<Object, Object> part1And2(List<String> lines) {
        List<List<Integer>> map = lines.stream().map(line -> {
                    List<Integer> l = new ArrayList<>();
                    char[] chars = line.toCharArray();
                    for (char aChar : chars) {
                        l.add(aChar - '0');
                    }
                    return l;
                }
        ).toList();
        Queue<Position> q = new LinkedList<>();
        for (int y = 0; y < map.size(); ++y) {
            for (int x = 0; x < map.get(y).size(); ++x) {
                if (map.get(y).get(x) == 0) {
                    Point2D start = new Point2D(x, y);
                    q.add(new Position(start, 0, start));
                }
            }
        }

        Set<Position> part1 = new HashSet<>();
        List<Position> part2 = new ArrayList<>();
        while (!q.isEmpty()) {
            Position position = q.poll();
            if (position.num == 9) {
                part1.add(position);
                part2.add(position);
                continue;
            }
            position.loc.neighboursCross()
                    .stream()
                    .filter(n -> n.getX() >= 0 && n.getY() >= 0 && n.getY() < map.size() && n.getX() < map.getFirst().size())
                    .filter(n -> map.get(n.getY()).get(n.getX()) == position.num + 1)
                    .forEach(p -> q.offer(new Position(p, position.num + 1, position.start)));
        }
        return new Pair<>(part1.size(), part2.size());
    }
}
