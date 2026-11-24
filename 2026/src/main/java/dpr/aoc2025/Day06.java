package dpr.aoc2026;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import dpr.commons.Day;
import dpr.commons.Util;

public class Day06 implements Day {

    @Override
    public int dayNum() {
        return 6;
    }

    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    long part1(List<String> lines) {
        return 0;
    }

    long part2(List<String> lines) {
        return 0;
    }

    public static void main(String[] args) {
        new Day06().execute();
    }
}
