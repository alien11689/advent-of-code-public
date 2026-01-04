package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class Day21Test {
    @Test
    void testPart1() {
        var result = Day21.part1(
                new Day21.Player(1, 3, 0),
                new Day21.Player(2, 7, 0));
        assertEquals(739785, result);
    }

    @Test
    void testPart2() {
        var result = Day21.part2(new Day21.PlayerV2(1, 3, 0, 1), new Day21.PlayerV2(2, 7, 0, 1));
        assertEquals(444356092776315L, result);
    }
}
