package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day14Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/14/test1.txt");
        var result = Day14.part1(input);
        assertEquals("165", result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/14/test2.txt");
        var result = Day14.part2(input);
        assertEquals("208", result);
    }
}
