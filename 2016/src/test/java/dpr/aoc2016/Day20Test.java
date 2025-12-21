package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day20Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/20/test1.txt");
        var result = Day20.part1(input);
        assertEquals(3, result);
    }
}
