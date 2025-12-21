package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day01Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/01/test1.txt");
        var result = Day01.part1(input);
        assertEquals(142, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/01/test2.txt");
        var result = Day01.part2(input);
        assertEquals(281, result);
    }
}
