package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day23Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/23/test1.txt");
        var result = Day23.part1(input);
        assertEquals(3, result);
    }
}
