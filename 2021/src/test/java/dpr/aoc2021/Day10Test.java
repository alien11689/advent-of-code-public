package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day10Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/10/test1.txt");
        var result = Day10.part1(input);
        assertEquals(26397, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/10/test1.txt");
        var result = Day10.part2(input);
        assertEquals(288957, result);
    }
}
