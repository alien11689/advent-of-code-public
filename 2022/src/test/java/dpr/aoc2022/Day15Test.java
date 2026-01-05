package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day15Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/15/test1.txt");
        var result = Day15.part1(input, 10);
        assertEquals(26, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/15/test1.txt");
        var result = Day15.part2(input, 20);
        assertEquals(56000011, result);
    }
}
