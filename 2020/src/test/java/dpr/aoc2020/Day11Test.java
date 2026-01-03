package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day11Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/11/test1.txt");
        var result = Day11.part1(input);
        assertEquals(37, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/11/test1.txt");
        var result = Day11.part2(input);
        assertEquals(26, result);
    }
}
