package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigInteger;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day15Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/15/test1.txt");
        var result = Day15.part1(input);
        assertEquals(40, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/15/test1.txt");
        var result = Day15.part2(input);
        assertEquals(315L, result);
    }
}
