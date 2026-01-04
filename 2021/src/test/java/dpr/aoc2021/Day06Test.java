package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day06Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/06/test1.txt");
        var result = Day06.part1And2(input, 80);
        assertEquals(5934L, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/06/test1.txt");
        var result = Day06.part1And2(input, 256);
        assertEquals(26984457539L, result);
    }
}
