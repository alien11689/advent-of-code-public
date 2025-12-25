package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class Day14Test {
    @Test
    void testPart1() {
        var result = Day14.part1("flqrgnkx");
        assertEquals(8108, result);
    }

    @Test
    void testPart2() {
        var result = Day14.part2("flqrgnkx");
        assertEquals(1242, result);
    }
}
