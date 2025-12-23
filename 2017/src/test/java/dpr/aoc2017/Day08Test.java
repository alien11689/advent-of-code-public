package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day08Test {
    @Test
    void testPart1And2() {
        var input = Util.getNotEmptyLinesFromFile("/08/test1.txt");
        var result = Day08.part1And2(input);
        assertEquals(2, result.size());
        assertEquals(1, result.get(0));
        assertEquals(10, result.get(1));
    }
}
