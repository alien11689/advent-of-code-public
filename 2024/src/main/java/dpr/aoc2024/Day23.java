package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class Day23 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        Pair<Object, Object> solution1And2 = part1And2(lines);
        System.out.println(solution1And2.first());
        System.out.println(solution1And2.second());
    }

    @Override
    public int dayNum() {
        return 23;
    }

    Pair<Object, Object> part1And2(List<String> lines) {
        Set<String> edges = new HashSet<>();
        Set<Set<String>> connections = lines.stream().map(line -> {
            String[] parts = line.split("-");
            Set<String> con = Set.of(parts[0], parts[1]);
            edges.addAll(con);
            return con;
        }).collect(Collectors.toSet());
        Set<List<String>> lans = new HashSet<>();
        edges.forEach(edge -> {
//            System.out.println(edge);
            List<String> connected = connections.stream()
                    .filter(c -> c.contains(edge))
                    .flatMap(Collection::stream)
                    .filter(e -> !e.equals(edge))
                    .sorted()
                    .toList();
            for (int i = 0; i < connected.size() - 1; i++) {
                for (int j = i + 1; j < connected.size(); j++) {
                    String a = connected.get(i);
                    String b = connected.get(j);
//                    System.out.println("Checking " + a + " and " + b);
                    Set<String> pair = Set.of(a, b);
                    if (connections.contains(pair)) {
                        List<String> list = new ArrayList<>(pair);
                        list.add(edge);
                        Collections.sort(list);
                        lans.add(list);
//                        System.out.println("connected: " + list);
                    }
                }
            }
        });
        long part1 = lans.stream().filter(lan -> lan.stream().anyMatch(e -> e.startsWith("t"))).count();
        int best = Integer.MIN_VALUE;
        String bestName = "";
        while (!lans.isEmpty()) {
            Set<String> cur = lans.stream().limit(1).flatMap(Collection::stream).collect(Collectors.toSet());
//            lans.remove(cur);
            while (true) {
                int prevSize = cur.size();
                Set<List<String>> toRemove = new HashSet<>();
                lans.forEach(l -> {
                    Set<String> intersection = new HashSet<>(l);
                    Set<String> left = new HashSet<>(l);
                    intersection.retainAll(cur);
                    if (intersection.size() == 3) {
                        toRemove.add(l);
                    } else if (intersection.size() == 2) {
                        left.removeAll(intersection);
                        String toCheck = left.stream().findAny().get();
                        if (cur.stream().allMatch(c -> {
                            Set<String> con = Set.of(c, toCheck);
                            return connections.contains(con);
                        })) {
                            cur.add(toCheck);
                            toRemove.add(l);
                        }
                    }
                });
                lans.removeAll(toRemove);
                if (prevSize == cur.size()) {
                    break;
                }
            }
            int size = cur.size();
            if (size > best) {
                best = size;
                bestName = cur.stream().sorted().collect(Collectors.joining(","));
            }
            lans.removeIf(lan -> lan.stream().anyMatch(cur::contains));
            connections.removeIf(con -> con.stream().anyMatch(cur::contains));
        }
        return new Pair<>(part1, bestName);
    }
}
