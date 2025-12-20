package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Point2D;

class Day13Test {
    @Test
    void testPart1() {
        var result = Day13.part1(10, new Point2D(7, 4));
        assertEquals(11, result);
    }
}
