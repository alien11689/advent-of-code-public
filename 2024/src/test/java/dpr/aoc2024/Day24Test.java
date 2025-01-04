package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day24Test {

    private final Day24 day = new Day24();

    @CsvSource({
            "test1.txt,4",
            "test2.txt,2024",
    })
    @ParameterizedTest
    void part1(String fileName, long expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), fileName);
        assertEquals(expected, day.part1(lines));
    }

}