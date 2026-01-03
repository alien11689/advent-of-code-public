package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day09Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/09/test1.txt")
                .stream()
                .map(Long::parseLong)
                .toList();
        var result = Day09.part1(input, 5);
        assertEquals(127, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/09/test1.txt")
                .stream()
                .map(Long::parseLong)
                .toList();
        var result = Day09.part2(input, 127);
        assertEquals(62, result);
    }
}
