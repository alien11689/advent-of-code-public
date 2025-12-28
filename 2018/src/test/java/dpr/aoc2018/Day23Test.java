package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day23Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/23/test1.txt");
        var result = Day23.part1(input);
        assertEquals(7, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/23/test2.txt");
        var result = Day23.part2(input);
        assertEquals(36, result);
    }
}
