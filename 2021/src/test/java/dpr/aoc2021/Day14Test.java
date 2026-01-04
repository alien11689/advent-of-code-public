package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigInteger;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day14Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/14/test1.txt");
        var result = Day14.part1(input);
        assertEquals(1588, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/14/test1.txt");
        var result = Day14.part2(input);
        assertEquals(BigInteger.valueOf(2188189693529L), result);
    }
}
