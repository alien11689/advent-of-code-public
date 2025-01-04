package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day01Test {

    private final Day01 day = new Day01();

    @Test
    void part1() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        Day01.Locations result = day.getLocations(lines);
        assertEquals(11, day.part1(result));
    }

    @Test
    void part2() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        Day01.Locations result = day.getLocations(lines);
        assertEquals(31, day.part2(result));
    }

}