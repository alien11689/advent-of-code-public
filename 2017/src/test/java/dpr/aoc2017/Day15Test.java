package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class Day15Test {
    @Test
    void testPart1() {
        var result = Day15.part1(65, 8921);
        assertEquals(588, result);
    }

    @Test
    void testPart2() {
        var result = Day15.part2(65, 8921);
        assertEquals(309, result);
    }
}
