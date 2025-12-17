package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day09Test {

    @Test
    void testPar1() {
        var lines = Util.getNotEmptyLinesFromFile("/09/test1.txt");
        var paths = Day09.readPaths(lines);
        int p1 = Day09.part1(paths);
        assertEquals(605, p1);
    }

    @Test
    void testPar2() {
        var lines = Util.getNotEmptyLinesFromFile("/09/test1.txt");
        var paths = Day09.readPaths(lines);
        int p2 = Day09.part2(paths);
        assertEquals(982, p2);
    }
}
