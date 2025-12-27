package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Point2D;
import dpr.commons.Util;

class Day22Test {
    @Test
    void testPart1() {
        var result = Day22.part1(new Point2D(10,10), 510);
        assertEquals(114, result);
    }

    @Test
    void testPart2() {
        var result = Day22.part2(new Point2D(10,10), 510);
        assertEquals(45, result);
    }
}
