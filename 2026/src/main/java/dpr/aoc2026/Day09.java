package dpr.aoc2026;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;

import org.jetbrains.annotations.NotNull;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Point2D;
import dpr.commons.Util;

public class Day09 implements Day {
    @Override
    public int dayNum() {
        return 9;
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
        new Day09().execute();
    }
}
