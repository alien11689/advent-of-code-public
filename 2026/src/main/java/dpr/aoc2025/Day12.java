package dpr.aoc2026;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

import org.jetbrains.annotations.NotNull;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Point2D;
import dpr.commons.Util;

public class Day12 implements Day {
    @Override
    public int dayNum() {
        return 12;
    }

    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
    }

    long part1(List<String> lines) {
        return 0;
    }

    public static void main(String[] args) {
        new Day12().execute();
    }
}
