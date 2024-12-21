package dpr.aoc2024;

import dpr.commons.Dir;
import dpr.commons.Point2D;
import dpr.commons.Util;
import kotlin.Pair;
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
import java.util.stream.Collectors;

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

    record SubPath(Point2D start, Point2D end, List<Character> clicks) {
    }

    private Object part1(List<String> lines) {
        long score = 0;
        int limit = 2;
        Map<Pair<SubPath, Integer>, Long> memo = new HashMap<>();
        for (String line : lines) {
            System.out.println("Line: " + line);
            List<Character> chars = line.chars()
                    .mapToObj(c -> (char) c)
                    .toList();
            Point2D level0Initial = new Point2D(2, 3); // A on numerical
            Set<List<SubPath>> paths0 = findPossiblePaths(level0Initial, chars, numeric);
            long best = Long.MAX_VALUE;
            int level = 0;
            for (List<SubPath> path0 : paths0) {
//                System.out.println(path0);
                long localBest = findBestLength(path0, level + 1, limit, memo);
//                System.out.println(localBest);
                if (localBest < best) {
                    best = localBest;
                }
            }
            score += best * Integer.parseInt(line.substring(0, 3));
        }
        System.out.println("Cache 1 - miss " + cache1Miss + ", hit " + cache1Hit);
        System.out.println("Cache 2 - miss " + cache2Miss + ", hit " + cache2Hit);
        return score;
    }

    private long findBestLength(List<SubPath> path, int level, int limit, Map<Pair<SubPath, Integer>, Long> memo) {
        long best = 0;
        for (SubPath subPath : path) {
            Set<List<SubPath>> possiblePaths = findPossiblePaths(new Point2D(2, 0), subPath.clicks, directional);
            if (level == limit) {
                int asInt = possiblePaths.stream().mapToInt(list -> list.stream().mapToInt(c -> c.clicks.size()).sum()).min().getAsInt();
                best += asInt;
            } else {
                best += possiblePaths.stream().mapToLong(pp -> findBestLength(pp, level + 1, limit, memo)).min().getAsLong();
            }
        }
        return best;
    }

    private static final Map<Pair<SubPath, Integer>, Long> directionalCache1 = new HashMap<>();
    private static int cache1Miss = 0;
    private static int cache1Hit = 0;

    record PositionNumeric(Point2D cur, int idx, List<SubPath> subPaths) {
    }

    @NotNull
    private Set<List<SubPath>> findPossiblePaths(Point2D initial, List<Character> chars, Map<Point2D, Character> board) {
        Set<List<SubPath>> paths = new HashSet<>();
        Queue<PositionNumeric> level0Queue = new LinkedList<>();
        level0Queue.offer(new PositionNumeric(initial, 0, new ArrayList<>()));
        while (!level0Queue.isEmpty()) {
            PositionNumeric pos = level0Queue.poll();
            if (pos.idx == chars.size()) {
                paths.add(pos.subPaths);
                continue;
            }
            char n = chars.get(pos.idx);
            Point2D targetPos = board.entrySet().stream().filter(e -> e.getValue() == n).findFirst().get().getKey();
            findPaths(pos.cur, targetPos, board).forEach(path -> {
                List<SubPath> subPaths = new ArrayList<>(pos.subPaths);
                List<Character> newClicks = path.stream().map(dir -> new Move(dir).toSymbol()).collect(Collectors.toList());
                newClicks.add('A');
                subPaths.add(new SubPath(pos.cur, targetPos, newClicks));
                level0Queue.offer(new PositionNumeric(targetPos, pos.idx + 1, subPaths));
            });
        }
        return paths;
    }

    record Path(Point2D cur, List<Dir> moves) {
    }

    private static final Map<Pair<Point2D, Point2D>, List<List<Dir>>> directionalCache2 = new HashMap<>();
    private static int cache2Miss = 0;
    private static int cache2Hit = 0;

    private Collection<List<Dir>> findPaths(Point2D start, Point2D targetPos, Map<Point2D, Character> board) {
        Pair<Point2D, Point2D> key = new Pair<>(start, targetPos);
        if (board == directional) {
            if (directionalCache2.containsKey(key)) {
                ++cache2Hit;
                return directionalCache2.get(key);
            }
        }
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
        if (board == directional) {
            ++cache2Miss;
            directionalCache2.put(key, result);
        }
        return result;
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
