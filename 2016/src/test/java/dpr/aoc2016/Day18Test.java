package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class Day18Test {
    @Test
    void testPart1() {
        var result = Day18.part1(".^^.^.^^^^", 10);
        assertEquals(38 , result);
    }
}
