package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Dir;
import dpr.commons.Pair;
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
import java.util.stream.Collectors;

class Day21 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
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

    record Move(Dir dir) {
        public char toSymbol() {
            return switch (dir) {
                case E -> '>';
                case W -> '<';
                case N -> '^';
                case S -> 'v';
            };
        }
    }

    record SubPath(Point2D start, Point2D end, List<Character> clicks) {
    }

    Object part1(List<String> lines) {
        return solve(lines, 2);
    }

    Object part2(List<String> lines) {
        return solve(lines, 25);
    }

    private long solve(List<String> lines, int limit) {
        memoMiss = 0;
        memoHit = 0;
        lowestLevelCacheMiss = 0;
        lowestLevelCacheHit = 0;
        long score = 0;
        Map<Pair<List<SubPath>, Integer>, Long> memo = new HashMap<>();
        for (String line : lines) {
//            System.out.println("Line: " + line);
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
//        System.out.println("Memo cache - miss " + memoMiss + ", hit " + memoHit);
//        System.out.println("Lowest level cache - miss " + lowestLevelCacheMiss + ", hit " + lowestLevelCacheHit);
        return score;
    }

    private long findBestLength(List<SubPath> path, int level, int limit, Map<Pair<List<SubPath>, Integer>, Long> memo) {
        long best = 0;
        Pair<List<SubPath>, Integer> key = new Pair<>(path, level);
        if (memo.containsKey(key)) {
            ++memoHit;
            return memo.get(key);
        }
        for (SubPath subPath : path) {
            Set<List<SubPath>> possiblePaths = findPossiblePaths(new Point2D(2, 0), subPath.clicks, directional);
            if (level == limit) {
                int asInt = possiblePaths.stream().mapToInt(list -> list.stream().mapToInt(c -> c.clicks.size()).sum()).min().getAsInt();
                best += asInt;
            } else {
                best += possiblePaths.stream().mapToLong(pp -> findBestLength(pp, level + 1, limit, memo)).min().getAsLong();
            }
        }
        memo.put(key, best);
        ++memoMiss;
        return best;
    }

    private static int memoMiss = 0;
    private static int memoHit = 0;

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

    private static final Map<Pair<Point2D, Point2D>, List<List<Dir>>> lowestLevelCache = new HashMap<>();
    private static int lowestLevelCacheMiss = 0;
    private static int lowestLevelCacheHit = 0;

    private Collection<List<Dir>> findPaths(Point2D start, Point2D targetPos, Map<Point2D, Character> board) {
        Pair<Point2D, Point2D> key = new Pair<>(start, targetPos);
        if (board == directional) {
            if (lowestLevelCache.containsKey(key)) {
                ++lowestLevelCacheHit;
                return lowestLevelCache.get(key);
            }
        }
        List<List<Dir>> result = new ArrayList<>();
        PriorityQueue<Path> q = new PriorityQueue<>(Comparator.comparingInt(o -> o.cur.manhattan(targetPos)));
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
            ++lowestLevelCacheMiss;
            lowestLevelCache.put(key, result);
        }
        return result;
    }
}
