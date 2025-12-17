package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day08Test {

    @Test
    void testPart1() {
        var lines = Util.getNotEmptyLinesFromFile("/08/test1.txt");
        int result = Day08.part1(lines);
        assertEquals(12, result);
    }

    @Test
    void testPart2() {
        var lines = Util.getNotEmptyLinesFromFile("/08/test1.txt");
        int result = Day08.part2(lines);
        assertEquals(19, result);
    }
}
