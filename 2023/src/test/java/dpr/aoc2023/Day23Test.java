package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day23Test {
    @Test
    void testPart1And2() {
        var input = Util.getNotEmptyLinesFromFile("/23/test1.txt");
        var result1 = Day23.part1(input);
        assertEquals(94, result1.getFirst());
        var result2 = Day23.part2(input, result1.component2());
        assertEquals(154, result2);
    }
}
