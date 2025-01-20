package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Day03 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 3;
    }

    Object part1(List<String> lines) {
        String program = String.join("", lines);
        return calculateSum(program);
    }

    private static long calculateSum(String program) {
        Pattern pattern = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)");
        Matcher matcher = pattern.matcher(program);
        var sum = 0L;
        while (matcher.find()) {
            long left = Integer.parseInt(matcher.group(1));
            long right = Integer.parseInt(matcher.group(2));
            sum += left * right;
        }
        return sum;
    }

    Object part2(List<String> lines) {
        String program = String.join("", lines);
        String[] split = program.split("((?=do(n't|)\\(\\))|(?<=do(n't|)\\(\\)))");
        boolean enabled = true;
        long sum = 0L;
        for (String cur : split) {
            if (cur.equals("do()")) {
                enabled = true;
            } else if (cur.equals("don't()")) {
                enabled = false;
            } else if (enabled) {
                sum += calculateSum(cur);
            }
        }
        return sum;
        //115739063 is too high
    }
}
