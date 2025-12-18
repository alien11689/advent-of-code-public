

package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day03Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/03/test1.txt");
        var result = Day03.part1(input);
        assertEquals(3, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/03/test1.txt");
        var result = Day03.part2(input);
        assertEquals(6, result);
    }
}
