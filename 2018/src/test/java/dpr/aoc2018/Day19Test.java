package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day19Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/19/test1.txt");
        var result = Day19.part1(input);
        assertEquals(6, result);
    }
}
