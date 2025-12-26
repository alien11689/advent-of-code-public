package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day17Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/17/test1.txt");
        var result = Day17.part1And2(input);
        assertEquals(57, result.getFirst());
        assertEquals(29, result.getSecond());
    }
}
