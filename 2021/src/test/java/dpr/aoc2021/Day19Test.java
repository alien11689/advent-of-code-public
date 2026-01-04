package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day19Test {
    @Test
    void testPart1And2() {
        var input = Util.getNotEmptyLinesFromFile("/19/test2.txt");
        var result = Day19.part1And2(input);
        assertEquals(2, result.size());
        assertEquals(79, result.get(0));
        assertEquals(3621, result.get(1));
    }
}
