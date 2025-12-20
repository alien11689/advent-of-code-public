package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Point2D;

class Day14Test {
    @Test
    void testPart1() {
        var result = Day14.part1("abc");
        assertEquals(22728, result);
    }

    @Test
    void testPart2() {
        var result = Day14.part2("abc");
        assertEquals(22551, result);
    }
}
