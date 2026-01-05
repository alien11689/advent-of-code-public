package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day25Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/25/test1.txt");
        var result = Day25.part1(input);
        assertEquals("2=-1=0", result);
    }
}
