package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day05Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/05/test1.txt");
        var result = Day05.part1(input);
        assertEquals(5, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/05/test1.txt");
        var result = Day05.part2(input);
        assertEquals(10, result);
    }
}
