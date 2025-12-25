package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day10Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/10/test1.txt");
        var result = Day10.part1And2(input);
        assertEquals(3, result.getSecond());
    }
}
