package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day07Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/07/test1.txt");
        var result = Day07.part1(input);
        assertEquals("CABDFE", result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/07/test1.txt");
        var result = Day07.part2(input, 2, 0);
        assertEquals(15, result);
    }
}
