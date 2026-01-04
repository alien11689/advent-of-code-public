package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day11Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/11/test2.txt");
        var result = Day11.part1(input);
        assertEquals(1656, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/11/test2.txt");
        var result = Day11.part2(input);
        assertEquals(195, result);
    }
}
