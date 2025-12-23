package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day07Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/07/test1.txt");
        var result = Day07.part1(input);
        assertEquals("tknk", result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/07/test1.txt");
        var result = Day07.part2(input);
        assertEquals(60, result);
    }
}
