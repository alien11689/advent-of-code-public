package dpr.aoc2026;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.jetbrains.annotations.NotNull;

import dpr.commons.Day;
import dpr.commons.Util;

public class Day05 implements Day {
    @Override
    public int dayNum() {
        return 5;
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
        new Day05().execute();
    }
}
