package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day19Test {

    @Test
    void testPart1_1() {
        var lines = Util.getFileContent("/19/test1.txt").lines().toList();
        var p1 = Day19.part1(lines);
        assertEquals(4, p1);
    }

    @Test
    void testPart1_2() {
        var lines = Util.getFileContent("/19/test2.txt").lines().toList();
        var p1 = Day19.part1(lines);
        assertEquals(7, p1);
    }

    @Test
    void testPart2_1() {
        var lines = Util.getFileContent("/19/test3.txt").lines().toList();
        var res = Day19.part2(lines);
        assertEquals(3, res);
    }

    @Test
    void testPart2_2() {
        var lines = Util.getFileContent("/19/test4.txt").lines().toList();
        var res = Day19.part2(lines);
        assertEquals(6, res);
    }
}
