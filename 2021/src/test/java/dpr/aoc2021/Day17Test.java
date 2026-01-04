package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;
import kotlin.ranges.IntRange;

class Day17Test {
    @Test
    void testPart1And2() {
        var result = Day17.part1And2(new Day17.Target(new IntRange(20,30), new IntRange(-10, -5)));
        assertEquals(2, result.size());
        assertEquals(45, result.get(0));
        assertEquals(112, result.get(1));
    }
}
