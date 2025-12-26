package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day18Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/18/test1.txt");
        var result = Day18.part1(input);
        assertEquals(1147, result);
    }
}
