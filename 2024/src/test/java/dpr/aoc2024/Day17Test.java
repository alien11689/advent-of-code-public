package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day17Test {

    private final Day17 day = new Day17();

    @CsvSource(value = {
            "test1.txt;4,6,3,5,6,3,5,2,1,0",
            "test2.txt;5,7,3,0",
    }, delimiter = ';')
    @ParameterizedTest
    void part1(String fileName, String expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), fileName);
        assertEquals(expected, day.part1(lines));
    }

    @Test
    void part2() {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), "test2.txt");
        assertEquals(117440L, day.part2(lines));
    }

}