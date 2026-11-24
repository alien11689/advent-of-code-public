package dpr.aoc2026;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.concurrent.atomic.LongAdder;
import java.util.stream.Collectors;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.IntExpr;
import dpr.commons.Day;
import dpr.commons.Util;

public class Day10 implements Day {
    @Override
    public int dayNum() {
        return 10;
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
        new Day10().execute();
    }
}
