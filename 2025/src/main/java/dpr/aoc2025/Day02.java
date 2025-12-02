package dpr.aoc2025;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import dpr.commons.Day;
import dpr.commons.Util;

class Day02 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 2;
    }

    long part1(List<String> lines) {
        long sum = 0L;
        for (String p : lines.get(0).split(",")) {
            String[] split = p.split("-");
            sum += checkRange1(split[0], split[1]);
        }
        return sum;
    }

    private long checkRange1(String leftString, String rightString) {
        long left = Long.parseLong(leftString);
        long right = Long.parseLong(rightString);
        int leftBeginLength = leftString.length() / 2;
        var minBegin = leftBeginLength < 1 ? 1L : Long.parseLong(leftString.substring(0, leftBeginLength));
        long sum = 0;
        while (true) {
            long l = Long.parseLong(minBegin + "" + minBegin);
            if (l > right) {
                break;
            }
            if (l >= left) {
                sum += l;
            }
            minBegin++;
        }
        return sum;
    }

    long part2(List<String> lines) {
        long sum = 0L;
        for (String p : lines.get(0).split(",")) {
            String[] split = p.split("-");
            sum += checkRange2(Long.parseLong(split[0]), Long.parseLong(split[1]));
        }
        return sum;
    }

    private long checkRange2(long left, long right) {
        long sum = 0;
        long cur = left;
        Set<Long> found = new HashSet<>();
        Set<String> checkedPrefix = new HashSet<>();
        int minDigits = String.valueOf(left).length();
        int maxDigits = String.valueOf(right).length();
        while (cur <= right) {
            String s = String.valueOf(cur);
            int substringSize = 1;
            while (substringSize * 2 <= maxDigits) {
                var begin = s.substring(0, substringSize);
                if (checkedPrefix.contains(begin)) {
                    ++substringSize;
                    continue;
                }
                checkedPrefix.add(begin);
                StringBuilder sb = new StringBuilder(begin);
                while (sb.length() <= maxDigits) {
                    if (sb.length() >= minDigits && sb.length() > 1) {
                        long l = Long.parseLong(sb.toString());
                        if (l >= left && l <= right && !found.contains(l)) {
                            found.add(l);
                            sum += l;
                        }
                    }
                    sb.append(begin);
                }
                ++substringSize;
            }
            ++cur;
        }
        return sum;
    }

    public static void main(String[] args) {
        new Day02().execute();
    }
}
