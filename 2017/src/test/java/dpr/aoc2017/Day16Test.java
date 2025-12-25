package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day16Test {
    @Test
    void testPart1() {
        var input = Util.getFileContent("/16/test1.txt").trim();
        var result = Day16.part1(input, 5);
        assertEquals("baedc", result);
    }
}
