package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day22Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/22/test1.txt");
        var result = Day22.part1(input);
        assertEquals(306, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/22/test1.txt");
        var result = Day22.part2(input);
        assertEquals(291, result);
    }
}
