package dpr.aoc2025;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day06Test {

    private final Day06 day = new Day06();

    @Test
    void part1() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(0, day.part1(lines));
    }

    @Test
    void part2() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(0, day.part2(lines));
    }

}