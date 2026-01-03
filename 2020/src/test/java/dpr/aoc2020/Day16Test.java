package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day16Test {
    @Test
    void testPart1() {
        var input = Util.getLinesFromFile("/16/test1.txt");
        var result = Day16.part1(input);
        assertEquals(4 + 55 + 12, result);
    }
}
