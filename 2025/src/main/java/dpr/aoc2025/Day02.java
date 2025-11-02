package dpr.aoc2025;

import java.util.List;

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

    int part1(List<String> lines) {
        return 0;
    }

    int part2(List<String> lines) {
        return 0;
    }

    public static void main(String[] args) {
        new Day02().execute();
    }
}
