package dpr.aoc2025;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import dpr.commons.Day;
import dpr.commons.Util;

class Day11 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 11;
    }

    long part1(List<String> lines) {
        var connections = readConnections(lines);
        return findUniquePaths("you", Set.of(), connections, new HashMap<>());
    }

    private static Map<String, Set<String>> readConnections(List<String> lines) {
        var connections = new HashMap<String, Set<String>>();
        for (String line : lines) {
            var parts = line.split("[: ]+");
            Set<String> targets = new HashSet<>();
            for (int i = 1; i < parts.length; ++i) {
                targets.add(parts[i]);
            }
            connections.put(parts[0], targets);
        }
        return connections;
    }

    long part2(List<String> lines) {
        var connections = readConnections(lines);
        return findUniquePaths("svr", Set.of("dac", "fft"), connections, new HashMap<>());
    }

    record State(String from, Set<String> mustVisit) {
    }

    private long findUniquePaths(String from, Set<String> mustVisit, Map<String, Set<String>> connections, Map<State, Long> memory) {
        var state = new State(from, mustVisit);
        Long fromCache = memory.get(state);
        if (fromCache != null) {
            return fromCache;
        }
        if (from.equals("out")) {
            long result = mustVisit.isEmpty() ? 1L : 0L;
            memory.put(state, result);
            return result;
        }
        var result = connections.getOrDefault(from, Set.of()).stream().mapToLong(next -> {
                    var newMustVisit = mustVisit;
                    if (mustVisit.contains(next)) {
                        newMustVisit = new HashSet<>(mustVisit);
                        newMustVisit.remove(next);
                    }
                    return findUniquePaths(next, newMustVisit, connections, memory);
                }
        ).sum();
        memory.put(state, result);
        return result;
    }

    public static void main(String[] args) {
        new Day11().execute();
    }
}
