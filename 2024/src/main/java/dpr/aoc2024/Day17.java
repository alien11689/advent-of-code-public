package dpr.aoc2024;

import dpr.commons.Util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

class Day17 implements Day {
    public static void main(String... args) {
        new Day17().execute(args);
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
        return 17;
    }

    private Object part1(List<String> lines) {
        int a = Integer.parseInt(lines.get(0).split(": ")[1]);
        int b = Integer.parseInt(lines.get(1).split(": ")[1]);
        int c = Integer.parseInt(lines.get(2).split(": ")[1]);
        List<Integer> program = Arrays.stream(lines.get(3).split(": ")[1].split(",")).map(Integer::parseInt).toList();
//        System.out.println(a);
//        System.out.println(b);
//        System.out.println(c);
//        System.out.println(program);
        int i = 0;
        List<Integer> result = new ArrayList<>();
        while (i < program.size()) {
            final int command = program.get(i);
            final int operand = program.get(i + 1);
            int realOperand;
            if (operand == 4) {
                realOperand = a;
            } else if (operand == 5) {
                realOperand = b;
            } else if (operand == 6) {
                realOperand = c;
            } else if (operand == 7) {
                throw new RuntimeException();
            } else {
                realOperand = operand;
            }
            if (command == 0) {
//                System.out.println("Running adv " + operand);
                int denom = (int) Math.pow(2, realOperand);
                a = a / denom;
            } else if (command == 1) {
//                System.out.println("Running bxl " + operand);
                b = b ^ operand;
            } else if (command == 2) {
//                System.out.println("Running bst " + operand);
                b = realOperand % 8;
            } else if (command == 3) {
//                System.out.println("Running jnz " + operand);
                if (a != 0) {
                    i = operand;
                    continue;
                }
            } else if (command == 4) {
//                System.out.println("Running bxc " + operand);
                b = b ^ c;
            } else if (command == 5) {
//                System.out.println("Running out " + operand);
                int val = realOperand % 8;
//                System.out.println("Printing " + val);
                result.add(val);
            } else if (command == 6) {
//                System.out.println("Running bdv " + operand);
                int denom = (int) Math.pow(2, realOperand);
                b = a / denom;
            } else if (command == 7) {
//                System.out.println("Running cdv " + operand);
                int denom = (int) Math.pow(2, realOperand);
                c = a / denom;
            }
            i += 2;
        }
        //1,1,6,6,2,6,3,2,4 wrong
        return result.stream().map(String::valueOf).collect(Collectors.joining(","));
    }

    private Object part2(List<String> lines) {
        return null;
    }
}
