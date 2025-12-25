package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day19Test {
    @Test
    void testPart1And2() {
        var input = Util.getNotEmptyLinesFromFile("/19/test1.txt");
        var result = Day19.part1And2(input);
        assertEquals("ABCDEF", result.getFirst());
        assertEquals(38, result.getSecond());
    }
}
