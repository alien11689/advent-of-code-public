package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day24Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var result = Day24.part1(input);
        assertEquals(10, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var result = Day24.part2(input);
        assertEquals(2208, result);
    }
}
