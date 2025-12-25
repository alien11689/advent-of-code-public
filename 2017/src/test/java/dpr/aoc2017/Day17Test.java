package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day17Test {
    @Test
    void testPart1() {
        var result = Day17.part1(3);
        assertEquals(638, result);
    }
}
