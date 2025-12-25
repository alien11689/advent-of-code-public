package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class Day08Test {
    @Test
    void testPart1() {
        var result = Day08.part1("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2");
        assertEquals(138, result);
    }

    @Test
    void testPart2() {
        var result = Day08.part2("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2");
        assertEquals(66, result);
    }
}
