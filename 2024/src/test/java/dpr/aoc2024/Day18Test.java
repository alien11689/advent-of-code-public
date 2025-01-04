package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day18Test {

    private final Day18 day = new Day18();

    @Test
    void part1() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(22, day.part1And2(lines, 6, 12).first());
    }

    @Test
    void part2() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals("6,1", day.part1And2(lines, 6, 12).second());
    }

}