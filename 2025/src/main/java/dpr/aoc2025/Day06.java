package dpr.aoc2025;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import dpr.commons.Day;
import dpr.commons.Util;

class Day06 implements Day {

    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 6;
    }

    long part1(List<String> lines) {
        long result = 0;
        List<List<Long>> numbers = new ArrayList<>();
        for (String line : lines) {
            String[] parts = line.trim().split(" +");
            if (line.startsWith("*") || line.startsWith("+")) {
                for (int i = 0; i < parts.length; i++) {
                    int finalI = i;
                    Stream<Long> operands = numbers.stream().map(l -> l.get(finalI));
                    if (parts[i].equals("*")) {
                        result += operands.reduce((a, b) -> a * b).get();
                    } else {
                        result += operands.reduce(Long::sum).get();
                    }
                }
            } else {
                numbers.add(Arrays.stream(parts).mapToLong(Long::parseLong).boxed().toList());
            }
        }
        return result;
    }

    long part2(List<String> lines) {
        int last = lines.stream().mapToInt(String::length).max().getAsInt();
        long result = 0;
        List<Long> nums = new ArrayList<>();
        while (last >= 0) {
            long num = 0;
            for (String line : lines) {
                char c = last < line.length() ? line.charAt(last) : ' ';
                if (c == ' ') {
                    if (num != 0) {
                        nums.add(num);
                        num = 0;
                    }
                } else if (Character.isDigit(c)) {
                    num = num * 10 + Integer.parseInt(c + "");
                } else {
                    if (num > 0) {
                        nums.add(num);
                    }
                    if (c == '+') {
                        result += nums.stream().reduce(Long::sum).get();
                    } else {
                        result += nums.stream().reduce((a, b) -> a * b).get();
                    }
                    nums.clear();
                    --last;
                }
            }
            --last;
        }
        return result;
    }


    public static void main(String[] args) {
        new Day06().execute();
    }
}
