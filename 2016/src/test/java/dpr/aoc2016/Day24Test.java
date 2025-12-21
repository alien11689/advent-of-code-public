package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day24Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var result = Day24.part1(input);
        assertEquals(14, result);
    }
}
