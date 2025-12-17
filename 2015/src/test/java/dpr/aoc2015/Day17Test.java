package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day17Test {

    @Test
    void testPart1() {
        var lines = Util.getNotEmptyLinesFromFile("/17/test1.txt");
        var containers = Day17.parseInput(lines);
        var p1 = Day17.part1(containers, 25);
        assertEquals(4, p1);
    }

    @Test
    void testPart2() {
        var lines = Util.getNotEmptyLinesFromFile("/17/test1.txt");
        var containers = Day17.parseInput(lines);
        var p2 = Day17.part2(containers, 25);
        assertEquals(3, p2);
    }
}
