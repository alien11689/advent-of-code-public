package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day20Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/20/test1.txt");
        var result = Day20.part1(input);
        assertEquals(1951L * 3079 * 2971 * 1171, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/20/test1.txt");
        var result = Day20.part2(input);
        assertEquals(273, result);
    }
}
