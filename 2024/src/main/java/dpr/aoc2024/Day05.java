package dpr.aoc2024;

import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class Day05 implements Day {
    public static void main(String... args) {
        new Day05().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 5;
    }

    record Rule(String a, String b) {
    }

    record Input(Set<List<String>> updates, Set<Rule> rules) {
    }

    private Object part1(List<String> lines) {
        Input input = parseInput(lines);
        return input.updates.stream()
                .filter(u -> matches(u, input.rules))
                .mapToInt(u -> Integer.parseInt(u.get((u.size() - 1) / 2)))
                .sum();
    }

    @NotNull
    private static Input parseInput(List<String> lines) {
        Set<Rule> rules = new HashSet<>();
        Set<List<String>> updates = new HashSet<>();
        lines.forEach(line -> {
            if (line.contains("|")) {
                String[] split = line.split("\\|");
                rules.add(new Rule(split[0], split[1]));
            } else {
                updates.add(Stream.of(line.split(",")).toList());
            }
        });
        return new Input(updates, rules);
    }

    private boolean matches(List<String> update, Set<Rule> rules) {
        var map = new HashMap<String, Integer>();
        for (int i = 0; i < update.size(); i++) {
            map.put(update.get(i), i);
        }
        for (Rule rule : rules) {
            if (map.containsKey(rule.a) && map.containsKey(rule.b)) {
                if (map.get(rule.a) > map.get(rule.b)) {
                    return false;
                }
            }
        }
        return true;
    }

    private Object part2(List<String> lines) {
        var input = parseInput(lines);
        List<List<String>> toFix = input.updates.stream()
                .filter(u -> !matches(u, input.rules))
                .toList();
        return toFix.stream()
                .mapToInt(u -> Integer.parseInt(fix(u, input.rules).get((u.size() - 1) / 2)))
                .sum();
    }

    private List<String> fix(List<String> u, Set<Rule> rules) {
        List<String> update = new ArrayList<>(u);
        Set<Rule> interestingRules = rules.stream().filter(r -> update.contains(r.a) && update.contains(r.b)).collect(Collectors.toSet());
        while (true) {
            boolean updated = false;
            for (Rule rule : interestingRules) {
                int va = update.indexOf(rule.a);
                int vb = update.indexOf(rule.b);
                if (va > vb) {
                    update.remove(rule.a);
                    update.add(vb, rule.a);
                    updated = true;
                }
            }
            if (!updated) {
                break;
            }
        }
        return update;
    }
}
