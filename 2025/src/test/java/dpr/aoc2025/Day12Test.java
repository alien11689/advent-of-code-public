package dpr.aoc2025;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import dpr.commons.Util;

class Day12Test {

    private final Day12 day = new Day12();

    @Test
    void part1() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(2, day.part1(lines));
    }
}