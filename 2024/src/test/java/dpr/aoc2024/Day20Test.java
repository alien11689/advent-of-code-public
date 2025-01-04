package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day20Test {

    private final Day20 day = new Day20();

    @CsvSource(value = {
            "64,2,1",
            "40,2,2",
            "39,2,2",
            "38,2,3",
            "20,2,5",
            "76,20,3",
            "74,20,7",
            "72,20,29",
    })
    @ParameterizedTest
    void part1(int threshold, int limit, int expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test1.txt");
        assertEquals(expected, day.part1And2(lines, threshold, limit));
    }

}