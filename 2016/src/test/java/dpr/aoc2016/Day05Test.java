package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class Day05Test {
    @Test
    void testPart1() {
        var result = Day05.part1("abc");
        assertEquals("18f47a30", result);
    }

    @Test
    void testPart2() {
        var result = Day05.part2("abc");
        assertEquals("05ace8e3", result);
    }
}
