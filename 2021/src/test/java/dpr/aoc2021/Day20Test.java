package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day20Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/20/test1.txt");
        var result = Day20.part1And2(input, 2);
        assertEquals(35, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/20/test1.txt");
        var result = Day20.part1And2(input, 50);
        assertEquals(3351, result);
    }
}
