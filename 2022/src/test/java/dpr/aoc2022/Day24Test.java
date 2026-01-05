package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day24Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var result = Day24.part1And2(input);
        assertEquals(2, result.size());
        assertEquals(18, result.get(0));
        assertEquals(54, result.get(1));
    }
}
