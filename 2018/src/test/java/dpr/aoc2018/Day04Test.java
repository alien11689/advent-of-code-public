package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day04Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/04/test1.txt");
        var result = Day04.part1(input);
        assertEquals(240, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/04/test1.txt");
        var result = Day04.part2(input);
        assertEquals(4455, result);
    }
}
