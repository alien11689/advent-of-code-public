package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day23Test {
    @Test
    void testPart1() {
        var result = Day23.part1("389125467");
        assertEquals("67384529", result);
    }

    @Test
    void testPart2() {
        var result = Day23.part2("389125467");
        assertEquals(149245887792L, result);
    }
}
