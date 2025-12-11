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
        result = findUniquePaths("you", connections, "out", Set.of());
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
        return findUniquePaths("svr", connections, "fft", Set.of()) *
                findUniquePaths("fft", connections, "dac", Set.of())*
                findUniquePaths("dac", connections, "out", Set.of());
    }

    long part2(List<String> lines) {
        var connections = readConnections(lines);
//        drawGraph(connections);
        var passes = new HashMap<Pair<String, String>, Long>();
        var hub1 = Set.of("wjw", "wzb", "ury", "lob", "bzl");
        for (String target : hub1) {
            var from = "svr";
            long uniquePaths = findUniquePaths(from, connections, target, hub1);
            passes.put(new Pair<>(from, target), uniquePaths);
//            System.out.println(from + " -> " + target + " paths: " + uniquePaths);
        }
        var hub2 = Set.of("vwz", "wef", "ych", "rpp");
        for (String from : hub1) {
            var target = "fft";
            long uniquePaths = findUniquePaths(from, connections, target, hub2);
            passes.put(new Pair<>(from, target), uniquePaths);
//            System.out.println(from + " -> " + target + " paths: " + uniquePaths);
        }
        // fft -> hub2
        for (String target : hub2) {
            var from = "fft";
            long uniquePaths = findUniquePaths(from, connections, target, hub2);
            passes.put(new Pair<>(from, target), uniquePaths);
//            System.out.println(from + " -> " + target + " paths: " + uniquePaths);
        }
        // hub2 -> hub3
        var hub3 = Set.of("pxb", "czt", "nhb");
        for (String from : hub2) {
            for (String target : hub3) {
                long uniquePaths = findUniquePaths(from, connections, target, hub3);
                passes.put(new Pair<>(from, target), uniquePaths);
//                System.out.println(from + " -> " + target + " paths: " + uniquePaths);
            }
        }

        // hub3 -> hub4
        var hub4 = Set.of("qqs", "wtz", "jnn", "fkf", "xal");
        for (String from : hub3) {
            for (String target : hub4) {
                long uniquePaths = findUniquePaths(from, connections, target, hub4);
                passes.put(new Pair<>(from, target), uniquePaths);
//                System.out.println(from + " -> " + target + " paths: " + uniquePaths);
            }
        }

        // hub4 -> dac excluding hub5
        var hub5 = Set.of("gui", "you", "wyy", "ydw", "sbh");
        for (String from : hub4) {
            var target = "dac";
            long uniquePaths = findUniquePaths(from, connections, target, hub5);
            passes.put(new Pair<>(from, target), uniquePaths);
//            System.out.println(from + " -> " + target + " paths: " + uniquePaths);
        }

        // dac -> hub5
        for (String target : hub5) {
            String from = "dac";
            long uniquePaths = findUniquePaths(from, connections, target, hub5);
            passes.put(new Pair<>(from, target), uniquePaths);
//            System.out.println(from + " -> " + target + " paths: " + uniquePaths);
        }

        // hub5 -> out
        for (String from : hub5) {
            var target = "out";
            long uniquePaths = findUniquePaths(from, connections, target, hub5);
            passes.put(new Pair<>(from, target), uniquePaths);
//            System.out.println(from + " -> " + target + " paths: " + uniquePaths);
        }

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

    private static void drawGraph(HashMap<String, Set<String>> connections) {
        System.out.println("digraph {");
        connections.forEach((from, value) -> {
            for (String target : value) {
                System.out.println(from + " -> " + target + (target.equals("fft") || target.equals("dac") ? " [color=\"red\"]" : ""));
            }
        });
        System.out.println("}");
    }

    private static long findUniquePaths(String origin, HashMap<String, Set<String>> connections, String target, Set<String> omitting) {
        PriorityQueue<List<String>> queue = new PriorityQueue<>(Comparator.comparing(List::size));
        var res = 0L;
        var path = List.of(origin);
        queue.offer(path);
        while (!queue.isEmpty()) {
//            System.out.println("Queue size is " + queue.size());
            List<String> cur = queue.poll();
            String last = cur.getLast();
            if(connections.containsKey(last)) {
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
