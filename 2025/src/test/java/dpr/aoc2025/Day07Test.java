package dpr.aoc2025;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day07Test {

    private final Day07 day = new Day07();

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