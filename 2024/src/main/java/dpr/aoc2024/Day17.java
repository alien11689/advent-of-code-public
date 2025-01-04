package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

class Day17 implements Day {
    public static void main(String... args) {
        new Day17().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
//            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "test1.txt");
//            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "test2.txt");
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
        List<Long> program = Arrays.stream(lines.get(3).split(": ")[1].split(",")).map(Long::parseLong).toList();
        List<Long> result = runProgram(program, a, b, c);
        //1,1,6,6,2,6,3,2,4 wrong
        return result.stream().map(String::valueOf).collect(Collectors.joining(","));
    }

    private Object part2(List<String> lines) {
//        int a = Integer.parseInt(lines.get(0).split(": ")[1]);
        long initB = Integer.parseInt(lines.get(1).split(": ")[1]);
        long initC = Integer.parseInt(lines.get(2).split(": ")[1]);
        List<Long> program = Arrays.stream(lines.get(3).split(": ")[1].split(",")).map(Long::parseLong).toList();
        long initA = 0L;
        int suffix = 4;
        List<Long> subProgram = program.subList(program.size() - suffix, program.size());
        while (true) {
            long a = ++initA;
            List<Long> result = runProgram(program, a, initB, initC);
            if (result.equals(subProgram)) {
//                String collect = result.stream().map(String::valueOf).collect(Collectors.joining(","));
//                System.out.println("Checking a = " + initA + " gives " + collect);
                if (result.equals(program)) {
                    return initA;
                }
                ++suffix;
                subProgram = program.subList(program.size() - suffix, program.size());
                initA *= 8L;
                initA -= 1;
            }
        }
    }

    @NotNull
    private static List<Long> runProgram(List<Long> program, long a, long b, long c) {
        int i = 0;
        List<Long> result = new ArrayList<>();
        while (i < program.size()) {
            final long command = program.get(i);
            final long operand = program.get(i + 1);
            long realOperand;
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
                    i = (int) operand;
                    continue;
                }
            } else if (command == 4) {
//                System.out.println("Running bxc " + operand);
                b = b ^ c;
            } else if (command == 5) {
//                System.out.println("Running out " + operand);
                long val = realOperand % 8;
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
        return result;
    }
}

/*
while (a !=0) {
b = b % a; // initially 0, then always in 0..7
b = b ^ 1 // last significant bit switched -> -1 if odd, +1 if even
c = a / 2^b //
a = a / 8 // a looses 3 most significant bits <--- the most important
b = b ^ 4  // 2 peniultimate bits are switched
b = b^c // b is 0 when b == c
out b
}
*/