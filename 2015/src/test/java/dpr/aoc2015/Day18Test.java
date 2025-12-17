package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day18Test {

    @Test
    void testExamplePart1() {
        var lines = Util.getNotEmptyLinesFromFile("/18/test1.txt");
        int res = Day18.part1(lines, 4);
        assertEquals(4, res);
    }

    @Test
    void testExamplePart2() {
        var lines = Util.getNotEmptyLinesFromFile("/18/test2.txt");
        int res = Day18.part2(lines, 5);
        assertEquals(17, res);
    }
}
