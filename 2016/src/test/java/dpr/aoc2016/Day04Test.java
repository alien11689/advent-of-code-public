package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day04Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/04/test1.txt");
        var result = Day04.part1(input);
        assertEquals(1514, result);
    }

    @Test
    void testPart2() {
        var line = new Day04.Line(List.of("qzmt", "zixmtkozy","ivhz"), 343, "z");
        assertEquals("veryencryptedname", line.decrypt());
    }
}
