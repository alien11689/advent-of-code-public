package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day08Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/08/test2.txt");
        var result = Day08.part1(input);
        assertEquals(26, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/08/test2.txt");
        var result = Day08.part2(input);
        assertEquals(61229, result);
    }
}
