package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day24Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var result = Day24.part1(Day24.chains(input));
        assertEquals(31, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var result = Day24.part2(Day24.chains(input));
        assertEquals(19, result);
    }
}
