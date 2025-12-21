package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day01Test {
    @Test
    void testPart1() {
        var input = Util.getLinesFromFile("/01/test1.txt");
        var result = Day01.part1(input);
        assertEquals(24000L, result);
    }

    @Test
    void testPart2() {
        var input = Util.getLinesFromFile("/01/test1.txt");
        var result = Day01.part2(input);
        assertEquals(45000L, result);
    }
}
