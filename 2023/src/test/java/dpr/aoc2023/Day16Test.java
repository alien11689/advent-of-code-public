package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day16Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/16/test1.txt");
        var result = Day16.part1(input);
        assertEquals(46, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/16/test1.txt");
        var result = Day16.part2(input);
        assertEquals(51, result);
    }
}
