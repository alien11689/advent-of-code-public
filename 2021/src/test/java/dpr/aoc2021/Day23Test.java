package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day23Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/23/test1.txt");
        var result = Day23.part1And2(input);
        assertEquals(12521, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/23/test2.txt");
        var result = Day23.part1And2(input);
        assertEquals(44169, result);
    }
}
