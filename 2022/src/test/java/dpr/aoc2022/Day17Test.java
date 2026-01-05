package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day17Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/17/test1.txt");
        var result = Day17.part1And2(input, 2022L, false);
        assertEquals(3068L, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/17/test1.txt");
        var result = Day17.part1And2(input, 1000000000000L, false);
        assertEquals(1514285714288L, result);
    }
}
