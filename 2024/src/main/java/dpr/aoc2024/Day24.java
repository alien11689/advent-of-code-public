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
                final String left = parts[0];
                final String right = parts[2];
                String target = parts[3];
                var rule = switch (parts[1]) {
                    case "AND" -> new And(left, right, target);
                    case "OR" -> new Or(left, right, target);
                    case "XOR" -> new Xor(left, right, target);
                    default -> throw new IllegalStateException("Unexpected value: " + parts[1]);
                };
                if (target.startsWith("z")) {
                    zTargets.add(target);
                }
                rules.add(rule);
            }
        });
        calculate(rules, inputs);
        return readNum(inputs, "z");
    }

    private static long readNum(Map<String, Boolean> inputs, String num) {
        String numString = inputs.entrySet().stream().filter(e -> e.getKey().startsWith(num))
                .sorted(Map.Entry.comparingByKey())
                .map(e -> e.getValue() ? "1" : "0")
                .collect(Collectors.joining());
        return Long.parseLong(new StringBuilder(numString).reverse().toString(), 2);
    }

    private Object part2(List<String> lines) {
        Set<Apply> rules = new HashSet<>();
        Map<String, Boolean> inputs = new HashMap<>();
        Set<String> zTargets = new HashSet<>();
        System.out.println("strict digraph {");
        lines.forEach(line -> {
            if (line.contains(":")) {
                String[] parts = line.split(": ");
                inputs.put(parts[0], "1".equals(parts[1]));
            } else {
                String[] parts = line.split("[ \\->]+");
                String left = parts[0];
                String right = parts[2];
                String target = parts[3];
                if (target.equals("z07")) {
                    target = "vmv";
                } else if (target.equals("vmv")) {
                    target = "z07";
                } else if (target.equals("z20")) {
                    target = "kfm";
                } else if (target.equals("kfm")) {
                    target = "z20";
                } else if (target.equals("z28")) {
                    target = "hnv";
                } else if (target.equals("hnv")) {
                    target = "z28";
                } else if (target.equals("hth")) {
                    target = "tqr";
                } else if (target.equals("tqr")) {
                    target = "hth";
                }
                var rule = switch (parts[1]) {
                    case "AND" -> new And(left, right, target);
                    case "OR" -> new Or(left, right, target);
                    case "XOR" -> new Xor(left, right, target);
                    default -> throw new IllegalStateException("Unexpected value: " + parts[1]);
                };
                System.out.println(left + " -> " + parts[1] + "_" + left + "_" + right);
                System.out.println(right + " -> " + parts[1] + "_" + left + "_" + right);
                System.out.println(parts[1] + "_" + left + "_" + right + " -> " + target);
                if (target.startsWith("z")) {
                    zTargets.add(target);
                }
                rules.add(rule);
            }
        });
        System.out.println("}");
        calculate(rules, inputs);
        System.out.println("Rules size: " + rules.size());
        long y = readNum(inputs, "y");
        long x = readNum(inputs, "x");
        long z = readNum(inputs, "z");
        show(x, y, z);
        List<String> list = Arrays.asList(
                "z07",
                "vmv",

                "z20",
                "kfm",

                "z28",
                "hnv",

                "hth",
                "tqr"
        );
        // input 2
        Map<String, Boolean> inputs2 = new HashMap<>();
        inputs.entrySet().forEach(e -> {
            if (e.getKey().startsWith("x")) {
                inputs2.put(e.getKey(), true);
            }
            if (e.getKey().startsWith("y")) {
                inputs2.put(e.getKey(), true);
            }
        });
        calculate(rules, inputs2);
        x = readNum(inputs2, "x");
        y = readNum(inputs2, "y");
        z = readNum(inputs2, "z");
        show(x, y, z);

        // input 3
        Map<String, Boolean> inputs3 = new HashMap<>();
        inputs.entrySet().forEach(e -> {
            if (e.getKey().startsWith("x")) {
                inputs3.put(e.getKey(), false);
            }
            if (e.getKey().startsWith("y")) {
                inputs3.put(e.getKey(), false);
            }
        });
        calculate(rules, inputs3);
        x = readNum(inputs3, "x");
        y = readNum(inputs3, "y");
        z = readNum(inputs3, "z");
        show(x, y, z);
        return list.stream().sorted().collect(Collectors.joining(","));
    }

    private static void show(long x, long y, long z) {
        System.out.println(x + " and " + y + " = " + (x + y) + " but is " + z);
        System.out.println(" " + Long.toBinaryString(x));
        System.out.println(" " + Long.toBinaryString(y));
        System.out.println(Long.toBinaryString(x + y));
        System.out.println(Long.toBinaryString(z));
    }

    private static void calculate(Set<Apply> rules, Map<String, Boolean> inputs) {
        while (true) {
//            System.out.println(rules);
            if (rules.stream().noneMatch(apply -> apply.calculate(inputs))) {
//                System.out.println(inputs);
//                System.out.println("Breaking");
                break;
            }
        }
    }
}
//00110111001110000111110010001110000
