package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day12Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/12/test1.txt");
        var result = Day12.part1(input);
        assertEquals(25, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/12/test1.txt");
        var result = Day12.part2(input);
        assertEquals(286, result);
    }
}
