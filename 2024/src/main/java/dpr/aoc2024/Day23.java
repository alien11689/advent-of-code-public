package dpr.aoc2024;

import dpr.commons.Util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

class Day23 implements Day {
    public static void main(String... args) {
        new Day23().execute(args);
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

    @Override
    public int dayNum() {
        return 23;
    }

    private Object part1(List<String> lines) {
        Set<String> edges = new HashSet<>();
        Set<List<String>> connections = lines.stream().map(line -> {
            String[] parts = line.split("-");
            List<String> list = Arrays.asList(parts);
            Collections.sort(list);
            edges.addAll(list);
            return list;
        }).collect(Collectors.toSet());
        Set<List<String>> lans = new HashSet<>();
        edges.stream().filter(e -> e.startsWith("t")).forEach(edge -> {
//            System.out.println(edge);
            List<String> connected = connections.stream().filter(c -> c.contains(edge)).flatMap(Collection::stream).filter(e -> !e.equals(edge)).sorted().toList();
            for (int i = 0; i < connected.size() - 1; i++) {
                for (int j = i + 1; j < connected.size(); j++) {
                    String a = connected.get(i);
                    String b = connected.get(j);
//                    System.out.println("Checking " + a + " and " + b);
                    List<String> list = new ArrayList<>(Arrays.asList(a, b));
                    if (connections.contains(list)) {
                        list.add(edge);
                        Collections.sort(list);
                        lans.add(list);
                    }
                }
            }
        });

        return lans.size();
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
