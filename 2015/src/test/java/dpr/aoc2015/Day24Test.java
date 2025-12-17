package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day24Test {

    @Test
    void testPart1() {
        var lines = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var numbers = Day24.parseInput(lines);
        var res = Day24.part1(numbers);
        assertEquals(99, res);
    }

    @Test
    void testPart2() {
        var lines = Util.getNotEmptyLinesFromFile("/24/test1.txt");
        var numbers = Day24.parseInput(lines);
        var res = Day24.part2(numbers);
        assertEquals(44, res);
    }
}
