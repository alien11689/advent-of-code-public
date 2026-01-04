package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day09Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/09/test1.txt");
        var result = Day09.part1(input);
        assertEquals(15, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/09/test1.txt");
        var result = Day09.part2(input);
        assertEquals(1134, result);
    }
}
