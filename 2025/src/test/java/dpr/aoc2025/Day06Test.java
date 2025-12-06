package dpr.aoc2025;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day06Test {

    private final Day06 day = new Day06();

    @Test
    void part1() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(4277556, day.part1(lines));
    }

    @Test
    void part2() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(3263827, day.part2(lines));
    }

}