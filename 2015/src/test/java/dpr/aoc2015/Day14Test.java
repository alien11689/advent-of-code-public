package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day14Test {

    @Test
    void testPart1() {
        var lines = Util.getNotEmptyLinesFromFile("/14/test1.txt");
        int p1 = Day14.part1(lines, 1000);
        assertEquals(1120, p1);
    }

    @Test
    void testPart2() {
        var lines = Util.getNotEmptyLinesFromFile("/14/test1.txt");
        int p2 = Day14.part2(lines, 1000);
        assertEquals(689, p2);
    }
}
