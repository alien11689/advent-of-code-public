package dpr.aoc2024;

import dpr.commons.Dir;
import dpr.commons.Point2D;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;

class Day21 implements Day {
    public static void main(String... args) {
        new Day21().execute(args);
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

    private static final Map<Point2D, Character> numeric = new HashMap<>();
    private static final Map<Point2D, Character> directional = new HashMap<>();

    static {
        numeric.put(new Point2D(0, 0), '7');
        numeric.put(new Point2D(1, 0), '8');
        numeric.put(new Point2D(2, 0), '9');
        numeric.put(new Point2D(0, 1), '4');
        numeric.put(new Point2D(1, 1), '5');
        numeric.put(new Point2D(2, 1), '6');
        numeric.put(new Point2D(0, 2), '1');
        numeric.put(new Point2D(1, 2), '2');
        numeric.put(new Point2D(2, 2), '3');
        numeric.put(new Point2D(1, 3), '0');
        numeric.put(new Point2D(2, 3), 'A');

        directional.put(new Point2D(1, 0), '^');
        directional.put(new Point2D(2, 0), 'A');
        directional.put(new Point2D(0, 1), '<');
        directional.put(new Point2D(1, 1), 'v');
        directional.put(new Point2D(2, 1), '>');
    }

    @Override
    public int dayNum() {
        return 21;
    }

    interface Click {
        char toSymbol();
    }

    record Move(Dir dir) implements Click {
        @Override
        public char toSymbol() {
            return switch (dir) {
                case E -> '>';
                case W -> '<';
                case N -> '^';
                case S -> 'v';
            };
        }
    }

    record Ack() implements Click {
        @Override
        public char toSymbol() {
            return 'A';
        }
    }

    record Position(Point2D cur, int idx, List<Click> clicks) {
    }

    private Object part1(List<String> lines) {
        long score = 0;
        for (String line : lines) {
            System.out.println("Line: " + line);
            List<Character> chars = line.chars()
                    .mapToObj(c -> (char) c)
                    .toList();
            Point2D level0Initial = new Point2D(2, 3); // A on numerical
            Point2D level1Initial = new Point2D(2, 0);
            Point2D level2Initial = new Point2D(2, 0);
            Set<List<Character>> paths0 = findPossiblePaths(level0Initial, chars, numeric);
            Set<List<Character>> paths1 = new HashSet<>();
            Set<List<Character>> paths2 = new HashSet<>();
            for (List<Character> path0 : paths0) {
//                System.out.println("Checking path0: " + path0);
                paths1.addAll(findPossiblePaths(level1Initial, path0, directional));
            }
            for (List<Character> path1 : paths1) {
//                System.out.println("Checking path1: " + path1);
                paths2.addAll(findPossiblePaths(level2Initial, path1, directional));
            }
            long best = paths2.stream().mapToInt(p2 -> p2.size()).min().getAsInt();
//            System.out.println(best);
            score += best * Integer.parseInt(line.substring(0, 3));
        }
        return score;
    }

    @NotNull
    private Set<List<Character>> findPossiblePaths(Point2D initial, List<Character> chars, Map<Point2D, Character> board) {
        Set<List<Character>> paths = new HashSet<>();
        Queue<Position> level0Queue = new LinkedList<>();
        level0Queue.offer(new Position(initial, 0, new ArrayList<>()));
        while (!level0Queue.isEmpty()) {
            Position pos = level0Queue.poll();
            if (pos.idx == chars.size()) {
                List<Character> clicks = pos.clicks.stream().map(Click::toSymbol).toList();
                paths.add(clicks);
                continue;
            }
            char n = chars.get(pos.idx);
            Point2D targetPos = board.entrySet().stream().filter(e -> e.getValue() == n).findFirst().get().getKey();
            findPaths(pos.cur, targetPos, board).forEach(path -> {
                List<Click> newClicks = new ArrayList<>(pos.clicks);
                path.stream().map(Move::new).forEach(newClicks::add);
                newClicks.add(new Ack());
                level0Queue.offer(new Position(targetPos, pos.idx + 1, newClicks));
            });
        }
        return paths;
    }

    record Path(Point2D cur, List<Dir> moves) {
    }

    private Collection<List<Dir>> findPaths(Point2D start, Point2D targetPos, Map<Point2D, Character> board) {
        List<List<Dir>> result = new ArrayList<>();
        PriorityQueue<Path> q = new PriorityQueue<>(new Comparator<Path>() {
            @Override
            public int compare(Path o1, Path o2) {
                return o1.cur.manhattan(targetPos) - o2.cur.manhattan(targetPos);
            }
        });
        q.offer(new Path(start, new ArrayList<>()));
        int best = Integer.MAX_VALUE;
        while (!q.isEmpty()) {
            Path path = q.poll();
//            System.out.println("Checking path: " + path);
            if (best < path.moves.size()) {
                continue;
            }
            if (path.cur.equals(targetPos)) {
                if (best > path.moves.size()) {
                    result = new ArrayList<>();
                    best = path.moves.size();
                }
                result.add(new ArrayList<>(path.moves));
            } else {
                Dir.getEntries().forEach(dir -> {
                    Point2D next = path.cur.move(dir, 1);
                    if (board.containsKey(next)) {
                        ArrayList<Dir> newPath = new ArrayList<>(path.moves);
                        newPath.add(dir);
                        q.offer(new Path(next, newPath));
                    }
                });
            }
        }
        return result;
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
