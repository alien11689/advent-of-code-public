package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day09Test {
    @Test
    void testPart1And2() {
        var input = Util.getNotEmptyLinesFromFile("/09/test1.txt");
        var result = Day09.part1And2(input);
        assertEquals(114, result.getFirst());
        assertEquals(2, result.getSecond());
    }
}
