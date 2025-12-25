package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day02Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/02/test1.txt");
        var result = Day02.part1(input);
        assertEquals(12, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/02/test2.txt");
        var result = Day02.part2(input);
        assertEquals("fgij", result);
    }
}
