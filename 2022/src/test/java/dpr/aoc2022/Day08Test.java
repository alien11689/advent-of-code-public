package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day08Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/08/test1.txt");
        var result = Day08.part1(input);
        assertEquals(21, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/08/test1.txt");
        var result = Day08.part2(input);
        assertEquals(8, result);
    }
}
