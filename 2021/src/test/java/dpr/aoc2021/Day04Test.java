package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day04Test {
    @Test
    void testPart1() {
        var input = Util.getLinesFromFile("/04/test1.txt");
        var result = Day04.part1(Day04.readInput(input));
        assertEquals(4512, result);
    }

    @Test
    void testPart2() {
        var input = Util.getLinesFromFile("/04/test1.txt");
        var result = Day04.part2(Day04.readInput(input));
        assertEquals(1924, result);
    }
}
