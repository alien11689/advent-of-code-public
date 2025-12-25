package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Set;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day21Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/21/test1.txt");
        var result = Day21.part1And2(input, Set.of(2));
        assertEquals(12, result.get(0));

    }
}
