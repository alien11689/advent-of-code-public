package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day23Test {

    @Test
    void testPart1() {
        var lines = Util.getNotEmptyLinesFromFile("/23/test1.txt");
        var instr = Day23.readInput(lines);
        var result = Day23.part1(instr, "a");
        assertEquals(2, result);
    }
}
