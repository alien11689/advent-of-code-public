package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day21Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/21/test1.txt");
        var result = Day21.part1(input, "abcde");
        assertEquals("decab", result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/21/test1.txt");
        var result = Day21.part2(input, "decab");
        assertEquals("abcde", result);
    }
}
