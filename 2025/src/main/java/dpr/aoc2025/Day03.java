package dpr.aoc2025;

import java.util.ArrayList;
import java.util.List;

import dpr.commons.Day;
import dpr.commons.Util;

public class Day03 implements Day {
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

    long part1(List<String> lines) {
        return lines.stream().mapToLong(line -> getBestBatteries(line, 2)).sum();
    }

    long part2(List<String> lines) {
        return lines.stream().mapToLong(line -> getBestBatteries(line, 12)).sum();
    }

    private long getBestBatteries(String line, int size) {
        List<String> batteries = new ArrayList<>();
        int missing = size;
        int curIndex = 0;
        while (missing > 0) {
            if (missing == line.length() - curIndex) {
                batteries.add(line.substring(curIndex));
                break;
            }
            for (int c = 9; c >= 0; --c) {
                int idx = line.indexOf("" + c, curIndex);
                if (idx == -1) {
                    continue;
                }
                if (idx + missing <= line.length()) {
                    curIndex = idx + 1;
                    --missing;
                    batteries.add("" + c);
                    break;
                }
            }
        }
        return Long.parseLong(String.join("", batteries));
    }

    public static void main(String[] args) {
        new Day03().execute();
    }
}
