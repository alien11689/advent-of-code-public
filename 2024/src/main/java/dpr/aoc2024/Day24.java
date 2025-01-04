package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

class Day24 implements Day {
    public static void main(String... args) {
        new Day24().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 24;
    }

    record Rule(Type type, String a, String b, String target) {
        boolean calculate(Map<String, Boolean> inputs) {
            if (inputs.containsKey(a) && inputs.containsKey(b) && !inputs.containsKey(target)) {
//                System.out.println("Calculated " + type + ": " + a + " " + b + " ->" + target);
                switch (type) {
                    case AND -> inputs.put(target, inputs.get(a) && inputs.get(b));
                    case OR -> inputs.put(target, inputs.get(a) || inputs.get(b));
                    case XOR -> inputs.put(target, inputs.get(a) && !inputs.get(b) || !inputs.get(a) && inputs.get(b));
                }
                return true;
            }
            return false;
        }

        Rule changeTarget(String target) {
            return new Rule(type, a, b, target);
        }

        boolean hasInputs(String i, String j) {
            return a().equals(i) && b().equals(j) || a().equals(j) && b().equals(i);
        }

    }

    enum Type {
        AND, OR, XOR
    }

    Object part1(List<String> lines) {
        Set<Rule> rules = new HashSet<>();
        Map<String, Boolean> inputs = new HashMap<>();
        parseInput(lines, inputs, rules);
        calculate(rules, inputs);
        return readNum(inputs, "z");
    }

    private static void parseInput(List<String> lines, Map<String, Boolean> inputs, Set<Rule> rules) {
//        System.out.println("strict digraph {");
        lines.forEach(line -> {
            if (line.contains(":")) {
                String[] parts = line.split(": ");
                inputs.put(parts[0], "1".equals(parts[1]));
            } else {
                String[] parts = line.split("[ \\->]+");
                final String left = parts[0];
                final String right = parts[2];
                String target = parts[3];
                var type = Type.valueOf(parts[1]);
//                System.out.println(left + " -> " + parts[1] + "_" + left + "_" + right);
//                System.out.println(right + " -> " + parts[1] + "_" + left + "_" + right);
//                System.out.println(parts[1] + "_" + left + "_" + right + " -> " + target);
                rules.add(new Rule(type, left, right, target));
            }
        });
//        System.out.println("}");
    }

    private static long readNum(Map<String, Boolean> inputs, String num) {
        String numString = inputs.entrySet().stream().filter(e -> e.getKey().startsWith(num))
                .sorted(Map.Entry.comparingByKey())
                .map(e -> e.getValue() ? "1" : "0")
                .collect(Collectors.joining());
        return Long.parseLong(new StringBuilder(numString).reverse().toString(), 2);
    }

    Object part2(List<String> lines) {
        Set<Rule> rules = new HashSet<>();
        Map<String, Boolean> inputs = new HashMap<>();
        parseInput(lines, inputs, rules);
        long zTargetCounts = rules.stream().filter(r -> r.target().startsWith("z")).count();
        Set<String> wrongTargets = findWrongTargets(rules, zTargetCounts);
        // rules swap detected manually
        rules = swapOutputs(rules, "z07", "vmv");
        rules = swapOutputs(rules, "z20", "kfm");
        rules = swapOutputs(rules, "z28", "hnv");
        rules = swapOutputs(rules, "tqr", "hth");
        calculate(rules, inputs);
        long y = readNum(inputs, "y");
        long x = readNum(inputs, "x");
        long z = readNum(inputs, "z");
//        show(x, y, z);
        if (x + y != z) {
            throw new IllegalStateException("Unexpected value: " + x + " + " + y + " = " + z + " (expected " + (x + y));
        }
        // input 2 - only 1
        long maxX = Long.parseLong("1".repeat((int) zTargetCounts - 1), 2);
        Map<String, Boolean> inputs2 = generateInput(maxX, maxX, zTargetCounts - 1);
        calculate(rules, inputs2);
        x = readNum(inputs2, "x");
        y = readNum(inputs2, "y");
        z = readNum(inputs2, "z");
//        show(x, y, z);
        if (x + y != z) {
            throw new IllegalStateException("Unexpected value: " + x + " + " + y + " = " + z + " (expected " + (x + y));
        }
        return wrongTargets.stream().sorted().collect(Collectors.joining(","));
    }

    @NotNull
    private static Set<String> findWrongTargets(Set<Rule> rules, long zTargetCounts) {
        // all Z outputs come from XOR
        // OR inputs are ANDs
        // no two AND in a row
        // no three XOR in a row - the middle one is invalid
        // single OR joins partial adders
        Set<String> wrongTargets = new HashSet<>();
        rules.forEach(rule -> {
            // last z can be OR
            if (rule.target.startsWith("z") && !rule.target().equals("z" + (zTargetCounts - 1))) {
                if (rule.type != Type.XOR) {
//                    System.out.println(rule + " has wrong target");
                    wrongTargets.add(rule.target());
                }
            }
            if (rule.type == Type.OR) {
                Rule ruleA = rules.stream().filter(r -> r.target().equals(rule.a)).findFirst().get();
                Rule ruleB = rules.stream().filter(r -> r.target().equals(rule.b)).findFirst().get();
                if (ruleA.type != Type.AND) {
//                    System.out.println(ruleA + " has wrong target");
                    wrongTargets.add(ruleA.target());
                }
                if (ruleB.type != Type.AND) {
//                    System.out.println(ruleB + " has wrong target");
                    wrongTargets.add(ruleB.target());
                }
            }
            if (rule.type == Type.AND) {
                // first AND can be used twice
                rules.stream().filter(r -> r.target().equals(rule.a)).findFirst().ifPresent(r -> {
                    if (r.type == Type.AND && !r.a.endsWith("00") && !r.b.endsWith("00")) {
//                        System.out.println(r + " has wrong target");
                        wrongTargets.add(r.target());
                    }
                });
                rules.stream().filter(r -> r.target().equals(rule.b)).findFirst().ifPresent(r -> {
                    if (r.type == Type.AND && !r.a.endsWith("00") && !r.b.endsWith("00")) {
//                        System.out.println(r + " has wrong target");
                        wrongTargets.add(r.target());
                    }
                });
            }
            if (rule.type == Type.XOR) {
                rules.stream().filter(r -> r.target().equals(rule.a)).findFirst().ifPresent(r -> {
                    if (r.type == Type.XOR) {
                        rules.stream().filter(ra -> ra.target().equals(r.a)).findFirst().ifPresent(ra -> {
//                            System.out.println(r + " has wrong target");
                            wrongTargets.add(r.target());
                        });
                        rules.stream().filter(ra -> ra.target().equals(r.b)).findFirst().ifPresent(ra -> {
//                            System.out.println(r + " has wrong target");
                            wrongTargets.add(r.target());
                        });
                    }
                });
                rules.stream().filter(r -> r.target().equals(rule.b)).findFirst().ifPresent(r -> {
                    if (r.type == Type.XOR) {
                        rules.stream().filter(ra -> ra.target().equals(r.a)).findFirst().ifPresent(ra -> {
//                            System.out.println(r + " has wrong target");
                            wrongTargets.add(r.target());
                        });
                        rules.stream().filter(ra -> ra.target().equals(r.b)).findFirst().ifPresent(ra -> {
//                            System.out.println(r + " has wrong target");
                            wrongTargets.add(r.target());
                        });
                    }
                });
            }
        });
        return wrongTargets;
    }

    private Map<String, Boolean> generateInput(long x, long y, long length) {
        String xBinary = new StringBuilder(Long.toBinaryString(x)).reverse().toString();
        String yBinary = new StringBuilder(Long.toBinaryString(y)).reverse().toString();
        Map<String, Boolean> result = new HashMap<>();
        for (int i = 0; i < length; i++) {
            result.put("x%02d".formatted(i), xBinary.length() > i && xBinary.charAt(i) == '1');
            result.put("y%02d".formatted(i), yBinary.length() > i && xBinary.charAt(i) == '1');
        }
        return result;
    }

    private Set<Rule> swapOutputs(Set<Rule> rules, String out1, String out2) {
        return rules.stream().map(r -> r.target().equals(out1) ? r.changeTarget(out2) : (r.target().equals(out2) ? r.changeTarget(out1) : r)).collect(Collectors.toSet());
    }

    private static void show(long x, long y, long z) {
        System.out.println(x + " and " + y + " = " + (x + y) + " but is " + z);
        System.out.println(" " + Long.toBinaryString(x));
        System.out.println(" " + Long.toBinaryString(y));
        System.out.println(Long.toBinaryString(x + y));
        System.out.println(Long.toBinaryString(z));
    }

    private static void calculate(Set<Rule> rules, Map<String, Boolean> inputs) {
        while (true) {
//            System.out.println(rules);
            if (rules.stream().noneMatch(rule -> rule.calculate(inputs))) {
//                System.out.println(inputs);
//                System.out.println("Breaking");
                break;
            }
        }
    }
}
//00110111001110000111110010001110000
