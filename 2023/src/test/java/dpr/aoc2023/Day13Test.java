package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day13Test {
    @Test
    void testPart1() {
        var input = Util.getLinesFromFile("/13/test1.txt");
        var result = Day13.part1(input);
        assertEquals(405, result);
    }

    @Test
    void testPart2() {
        var input = Util.getLinesFromFile("/13/test1.txt");
        var result = Day13.part2(input);
        assertEquals(400, result);
    }
}
