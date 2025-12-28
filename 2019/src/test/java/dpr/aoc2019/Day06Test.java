package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day06Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/06/test1.txt");
        var result = Day06.part1(input);
        assertEquals(42, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/06/test2.txt");
        var result = Day06.part2(input);
        assertEquals(4, result);
    }
}
