package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day05Test {
    @Test
    void testPart1() {
        var result = Day05.part1("dabAcCaCBAcCcaDA");
        assertEquals(10, result);
    }

    @Test
    void testPart2() {
        var result = Day05.part2("dabAcCaCBAcCcaDA");
        assertEquals(4, result);
    }
}
