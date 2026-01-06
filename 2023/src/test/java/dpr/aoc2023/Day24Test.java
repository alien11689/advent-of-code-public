package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day24Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var result = Day24.part1(input, 7L, 27L);
        assertEquals(2, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var result = Day24.part2(input);
        assertEquals("47", result);
    }
}
