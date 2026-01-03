package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day17Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/17/test1.txt");
        var result = Day17.part1(input);
        assertEquals(112, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/17/test1.txt");
        var result = Day17.part2(input);
        assertEquals(848, result);
    }
}
