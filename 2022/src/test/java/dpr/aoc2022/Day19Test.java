package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day19Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/19/test1.txt");
        var result = Day19.part1(input);
        assertEquals(33, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/19/test1.txt");
        var result = Day19.part2(input);
        assertEquals(56 * 62, result);
    }
}
