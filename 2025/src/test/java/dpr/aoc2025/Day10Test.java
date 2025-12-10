package dpr.aoc2025;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import dpr.commons.Util;

class Day10Test {

    private final Day10 day = new Day10();

    @Test
    void part1() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(7, day.part1(lines));
    }

    @ParameterizedTest
    @EnumSource(Day10.SolverAlgorithm.class)
    void part2(Day10.SolverAlgorithm solverAlgorithm) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(33, day.part2(lines, solverAlgorithm));
    }

}