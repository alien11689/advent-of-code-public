package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day25Test {

    private final Day25 day = new Day25();

    @Test
    void part1() {
        var lines = Util.getLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(3, day.part1(lines));
    }

}