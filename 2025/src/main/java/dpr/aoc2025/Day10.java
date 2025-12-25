package dpr.aoc2025;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.concurrent.atomic.LongAdder;
import java.util.stream.Collectors;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.IntExpr;
import dpr.commons.Day;
import dpr.commons.Util;

public class Day10 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines, SolverAlgorithm.Z3));
    }

    @Override
    public int dayNum() {
        return 10;
    }

    enum SolverAlgorithm {
        BRUTE_FORCE,
        Z3
    }

    long part1(List<String> lines) {
        long result = 0;
        for (String line : lines) {
            String[] parts = line.split(" ");
            var target = parts[0].substring(1, parts[0].length() - 1).chars().mapToObj(c -> c == '#').collect(Collectors.toList());
            var buttons = new ArrayList<Set<Integer>>();
            for (int i = 1; i < parts.length - 1; i++) {
                var button = Arrays.stream(parts[i].substring(1, parts[i].length() - 1).split(",")).map(Integer::parseInt).collect(Collectors.toSet());
                buttons.add(button);
            }
            var pq = new PriorityQueue<State1>();
            pq.offer(new State1(0, target.stream().map(v -> false).toList()));
            var history = new HashSet<List<Boolean>>();
            end:
            while (!pq.isEmpty()) {
                State1 cur = pq.poll();
                if (history.contains(cur.view)) {
                    continue;
                }
                history.add(cur.view);
                for (Set<Integer> button : buttons) {
                    var next = cur.push(button);
                    if (target.equals(next.view)) {
                        result += next.steps;
                        break end;
                    }
                    if (!history.contains(next)) {
                        pq.offer(next);
                    }
                }
            }
        }
        return result;
    }

    record State1(int steps, List<Boolean> view) implements Comparable<State1> {
        @Override
        public int compareTo(State1 other) {
            return Integer.compare(this.steps, other.steps);
        }

        public State1 push(Set<Integer> button) {
            var newView = new ArrayList<>(view);
            for (Integer idx : button) {
                newView.set(idx, !newView.get(idx));
            }
            return new State1(steps + 1, newView);
        }
    }

    long part2(List<String> lines, SolverAlgorithm solverAlgorithm) {
        var result = new LongAdder();
        for (int i = 0; i < lines.size(); i++) {
//            System.out.println((i + 1) + "/" + lines.size() + ": " + lines.get(i));
            var line = lines.get(i);
            result.add(
                    switch (solverAlgorithm) {
                        case BRUTE_FORCE -> solveLineWithBruteForce(line);
                        case Z3 -> solveLineWithZ3(line);
                    });
        }
        return result.longValue();
    }

    private static long solveLineWithBruteForce(String line) {
        String[] parts = line.split(" ");
        String lastPart = parts[parts.length - 1];
        String[] splitIndicators = lastPart.substring(1, lastPart.length() - 1).split(",");
        var target = Arrays.stream(splitIndicators).map(Integer::parseInt).collect(Collectors.toList());
//        System.out.println(target);
        var buttons = new ArrayList<Set<Integer>>();
        for (int i = 1; i < parts.length - 1; i++) {
            var button = Arrays.stream(parts[i].substring(1, parts[i].length() - 1).split(",")).map(Integer::parseInt).collect(Collectors.toSet());
            buttons.add(button);
        }
        buttons.sort((o1, o2) -> o2.size() - o1.size());

//        System.out.println("Button analysis: " + bestIndexToOptimize);

//        System.out.println(buttons);
        var pq = new PriorityQueue<State2>();
        pq.offer(new State2(0, target, buttons));
        var best = Long.MAX_VALUE;

        while (!pq.isEmpty()) {
            var cur = pq.poll();
//                System.out.println("Checking " + cur);
            if (cur.steps >= best) {
                continue;
            }
            for (var next : cur.generateNext()) {
                if (next.steps >= best) {
                    continue;
                }
                if (next.sum == 0) {
//                    System.out.println("Updating the best to " + next.steps);
                    best = next.steps;
//                    System.out.println("PQ size: " + pq.size());
                } else {
                    if (!next.buttons.isEmpty() && next.steps < best) {
                        pq.offer(next);
                    }
                }
            }
        }
//        System.out.println("Solve with best: " + best);
        return best;
    }

    private static Integer getBestIndexToOptimize(List<Set<Integer>> buttons) {
        var map = new HashMap<Integer, Integer>();
        for (var button : buttons) {
            for (int i : button) {
                map.put(i, map.getOrDefault(i, 0) + 1);
            }
        }
        //        System.out.println("Button analysis: " + map);
        return map.entrySet().stream().min(Map.Entry.comparingByValue()).get().getKey();
    }

    record State2(int steps, List<Integer> view, List<Set<Integer>> buttons, int sum) implements Comparable<State2> {
        State2(int steps, List<Integer> view, List<Set<Integer>> buttons) {
            this(steps, view, buttons, view.stream().mapToInt(i -> i).sum());
        }

        @Override
        public int compareTo(State2 other) {
            int compareSum = Integer.compare(this.sum, other.sum);
            if (compareSum == 0) {
                return Integer.compare(this.steps, other.steps);
            }
            return compareSum;
        }

        public List<State2> generateNext() {
            if (buttons.isEmpty()) {
                return List.of();
            }

            // not calculating when the state is not achievable
            var possible = new HashSet<>();
            for (var button : buttons) {
                possible.addAll(button);
            }
            for (int i = 0; i < view.size(); i++) {
                if (view.get(i) > 0 && !possible.contains(i)) {
                    return List.of();
                }
            }

            // choosing the best button
            var bestIndexToOptimize = getBestIndexToOptimize(buttons);
            var button = buttons.stream().filter(b -> b.contains(bestIndexToOptimize)).findFirst().get();
            var nextButtons = buttons.stream().filter(b -> b != button).toList();

            // generating next states
//            var button = buttons.getFirst();
//            var nextButtons = buttons.subList(1, buttons.size());
            var result = new ArrayList<State2>();
            var times = 0;
            while (true) {
                var newView = new ArrayList<>(view);
                for (Integer idx : button) {
                    newView.set(idx, newView.get(idx) - times);
                }
                if (newView.stream().noneMatch(v -> v < 0)) {
                    result.add(new State2(steps + times, newView, nextButtons));
                } else {
                    break;
                }
                ++times;
            }
            return result;
        }
    }

    private long solveLineWithZ3(String line) {
        String[] parts = line.split(" ");
        String lastPart = parts[parts.length - 1];
        String[] splitIndicators = lastPart.substring(1, lastPart.length() - 1).split(",");
        var target = Arrays.stream(splitIndicators).map(Integer::parseInt).collect(Collectors.toList());
//        System.out.println(target);
        var buttons = new ArrayList<Set<Integer>>();
        for (int i = 1; i < parts.length - 1; i++) {
            var button = Arrays.stream(parts[i].substring(1, parts[i].length() - 1).split(",")).map(Integer::parseInt).collect(Collectors.toSet());
            buttons.add(button);
        }

        try (var ctx = new Context()) {
            var solver = ctx.mkOptimize();
            var vars = new ArrayList<IntExpr>();
            var booleanExpr = new ArrayList<BoolExpr>();
            var zero = ctx.mkInt(0);
            var targetParts = target.stream().map(v -> new ArrayList<IntExpr>()).toList();
            for (int b = 0; b < buttons.size(); b++) {
                var v = ctx.mkIntConst("b" + b);
                vars.add(v);
                booleanExpr.add(ctx.mkGe(v, zero));
                for (int button : buttons.get(b)) {
                    targetParts.get(button).add(v);
                }
            }
            for (int i = 0; i < targetParts.size(); i++) {
                booleanExpr.add(ctx.mkEq(ctx.mkAdd(targetParts.get(i).toArray(new IntExpr[0])), ctx.mkInt(target.get(i))));
            }
            var constraints = ctx.mkAnd(booleanExpr.toArray(new BoolExpr[0]));
            var sum = ctx.mkAdd(vars.toArray(new IntExpr[0]));

            solver.Assert(constraints);
            solver.MkMinimize(sum);
            solver.Check();
            long best = Long.parseLong(solver.getModel().eval(sum, true).toString());
//            System.out.println("Solve with best: " + best);
            return best;
        }
    }

    public static void main(String[] args) {
        new Day10().execute();
    }
}
