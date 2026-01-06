package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day22Test {
    @Test
    void testPart1And2() {
        var input = Util.getNotEmptyLinesFromFile("/22/test1.txt");
        var result = Day22.part1And2(input);
        assertEquals(5, result.getFirst());
        assertEquals(7, result.getSecond());
    }
}
