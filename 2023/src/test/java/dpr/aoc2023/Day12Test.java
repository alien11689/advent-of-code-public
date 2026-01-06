package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day12Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/12/test1.txt");
        var result = Day12.part1(input);
        assertEquals(21, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/12/test1.txt");
        var result = Day12.part2(input);
        assertEquals(525152, result);
    }
}
