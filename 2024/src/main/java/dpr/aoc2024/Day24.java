package dpr.aoc2024;

import dpr.commons.Util;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

class Day24 implements Day {
    public static void main(String... args) {
        new Day24().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test2.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 24;
    }

    interface Apply {
        boolean calculate(Map<String, Boolean> inputs);
    }

    record And(String a, String b, String target) implements Apply {
        @Override
        public boolean calculate(Map<String, Boolean> inputs) {
            if (inputs.containsKey(a) && inputs.containsKey(b) && !inputs.containsKey(target)) {
                inputs.put(target, inputs.get(a) && inputs.get(b));
                return true;
            }
            return false;
        }
    }

    record Or(String a, String b, String target) implements Apply {
        @Override
        public boolean calculate(Map<String, Boolean> inputs) {
            if (inputs.containsKey(a) && inputs.containsKey(b) && !inputs.containsKey(target)) {
                inputs.put(target, inputs.get(a) || inputs.get(b));
                return true;
            }
            return false;
        }
    }

    record Xor(String a, String b, String target) implements Apply {
        @Override
        public boolean calculate(Map<String, Boolean> inputs) {
            if (inputs.containsKey(a) && inputs.containsKey(b) && !inputs.containsKey(target)) {
                inputs.put(target, inputs.get(a) && !inputs.get(b) || !inputs.get(a) && inputs.get(b));
                return true;
            }
            return false;
        }
    }

    private Object part1(List<String> lines) {
        Set<Apply> rules = new HashSet<>();
        Map<String, Boolean> inputs = new HashMap<>();
        Set<String> zTargets = new HashSet<>();
        lines.forEach(line -> {
            if (line.contains(":")) {
                String[] parts = line.split(": ");
                inputs.put(parts[0], "1".equals(parts[1]));
            } else {
                String[] parts = line.split("[ \\->]+");
                var rule = switch (parts[1]) {
                    case "AND" -> new And(parts[0], parts[2], parts[3]);
                    case "OR" -> new Or(parts[0], parts[2], parts[3]);
                    case "XOR" -> new Xor(parts[0], parts[2], parts[3]);
                    default -> throw new IllegalStateException("Unexpected value: " + parts[1]);
                };
                if (parts[3].startsWith("z")) {
                    zTargets.add(parts[3]);
                }
                rules.add(rule);
            }
        });
        while (true) {
//            System.out.println(rules);
            if (rules.stream().noneMatch(apply -> apply.calculate(inputs))) {
//                System.out.println(inputs);
//                System.out.println("Breaking");
                break;
            }
        }
        String numString = inputs.entrySet().stream().filter(e -> e.getKey().startsWith("z"))
                .sorted(Map.Entry.comparingByKey())
                .map(e -> e.getValue() ? "1" : "0")
                .collect(Collectors.joining());
        return Long.parseLong(new StringBuilder(numString).reverse().toString(), 2);
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
