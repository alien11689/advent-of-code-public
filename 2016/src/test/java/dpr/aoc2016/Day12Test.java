
package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Set;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day12Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/12/test1.txt");
        var result = Day12.part1(input);
        assertEquals(42, result);
    }
}
