package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class Day19Test {
    @Test
    void testPart1() {
        var result = Day19.part1(5);
        assertEquals(3, result);
    }

    @Test
    void testPart2() {
        var result = Day19.part2(5);
        assertEquals(2, result);
    }
}
