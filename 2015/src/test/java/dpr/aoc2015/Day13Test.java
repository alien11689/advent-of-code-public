package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day13Test {

    @Test
    void testPart1() {
        var lines = Util.getNotEmptyLinesFromFile("/13/test1.txt");
        var paths = Day13.readPaths(lines);
        int res = Day13.part1(paths);
        assertEquals(330, res);
    }
}
