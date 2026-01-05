package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day21Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/21/test1.txt");
        var result = Day21.part1(input);
        assertEquals(152, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/21/test1.txt");
        var result = Day21.part2(input);
        assertEquals(301, result);
    }
}
