package dpr.aoc2025;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;

import dpr.commons.Day;
import dpr.commons.Pair;
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
        var result = 0L;
        var connections = readConnections(lines);
        result = findUniquePaths("you", "out", Set.of(), connections);
        return result;
    }

    private static HashMap<String, Set<String>> readConnections(List<String> lines) {
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

    long part2Naive(List<String> lines) {
        var connections = readConnections(lines);
        System.out.println(connections);
        return findUniquePaths("svr", "fft", Set.of(), connections) *
                findUniquePaths("fft", "dac", Set.of(), connections) *
                findUniquePaths("dac", "out", Set.of(), connections);
    }

    long part2(List<String> lines) {
        var connections = readConnections(lines);
//        drawGraph(connections);
        var passes = new HashMap<Pair<String, String>, Long>();
        var hub1 = Set.of("wjw", "wzb", "ury", "lob", "bzl");
        var fft = Set.of("fft");
        var hub2 = Set.of("vwz", "wef", "ych", "rpp");
        var hub3 = Set.of("pxb", "czt", "nhb");
        var hub4 = Set.of("qqs", "wtz", "jnn", "fkf", "xal");
        var dac = Set.of("dac");
        var hub5 = Set.of("gui", "you", "wyy", "ydw", "sbh");
        var out = Set.of("out");
        findGraphPasses(Set.of("svr"), hub1, hub1, connections, passes);
        findGraphPasses(hub1, fft, hub2, connections, passes);
        findGraphPasses(fft, hub2, hub2, connections, passes);
        findGraphPasses(hub2, hub3, hub3, connections, passes);
        findGraphPasses(hub3, hub4, hub4, connections, passes);
        findGraphPasses(hub4, dac, hub5, connections, passes);
        findGraphPasses(dac, hub5, hub5, connections, passes);
        findGraphPasses(hub5, out, out, connections, passes);

        var result = 0L;
        var queue = new LinkedList<Pair<List<String>, Long>>();
        queue.offer(new Pair<>(List.of("svr"), 1L));
        while (!queue.isEmpty()) {
            Pair<List<String>, Long> cur = queue.poll();
            for (Map.Entry<Pair<String, String>, Long> pass : passes.entrySet()) {
                if (pass.getKey().first().equals(cur.first().getLast())) {
                    var subres = cur.second() * pass.getValue();
                    var next = pass.getKey().second();
                    if ("out".equals(next)) {
                        result += subres;
                        continue;
                    }
                    var nextPath = new ArrayList<>(cur.first());
                    nextPath.add(next);
                    queue.offer(new Pair<>(nextPath, subres));
                }
            }
        }
        return result;
    }

    private static void findGraphPasses(Set<String> froms, Set<String> targets, Set<String> excluding, Map<String, Set<String>> connections, Map<Pair<String, String>, Long> passes) {
        for (String from : froms) {
            for (String target : targets) {
                long uniquePaths = findUniquePaths(from, target, excluding, connections);
                passes.put(new Pair<>(from, target), uniquePaths);
//                System.out.println(from + " -> " + target + " paths: " + uniquePaths);
            }
        }
    }

    private static void drawGraph(HashMap<String, Set<String>> connections) {
        System.out.println("digraph {");
        connections.forEach((from, value) -> {
            for (String target : value) {
                System.out.println(from + " -> " + target + (target.equals("fft") || target.equals("dac") ? " [color=\"red\"]" : ""));
            }
        });
        System.out.println("}");
    }

    private static long findUniquePaths(String origin, String target, Set<String> omitting, Map<String, Set<String>> connections) {
        PriorityQueue<List<String>> queue = new PriorityQueue<>(Comparator.comparing(List::size));
        var res = 0L;
        var path = List.of(origin);
        queue.offer(path);
        while (!queue.isEmpty()) {
//            System.out.println("Queue size is " + queue.size());
            List<String> cur = queue.poll();
            String last = cur.getLast();
            if (connections.containsKey(last)) {
                for (String next : connections.get(last)) {
                    if (target.equals(next)) {
                        ++res;
                        continue;
                    }
                    if (cur.contains(next)) {
                        continue;
                    }
                    if (omitting.contains(next)) {
                        continue;
                    }
                    var newPath = new ArrayList<>(cur);
                    newPath.add(next);
                    queue.offer(newPath);
                }
            }
        }
        return res;
    }

    public static void main(String[] args) {
        new Day11().execute();
    }
}
