package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day08Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/08/test1.txt");
        var result = Day08.part1And2(input, 6, 2);
        assertEquals(6, result.getFirst());
    }
}
