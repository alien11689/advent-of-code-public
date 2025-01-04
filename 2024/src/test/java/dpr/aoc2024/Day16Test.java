package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day16Test {

    private final Day16 day = new Day16();

    @CsvSource({
            "test1.txt,7036",
            "test2.txt,11048",
    })
    @ParameterizedTest
    void part1(String fileName, int expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), fileName);
        assertEquals(expected, day.part1And2(lines).first());
    }

    @CsvSource({
            "test1.txt,45",
            "test2.txt,64"
    })
    @ParameterizedTest
    void part2(String fileName, int expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), fileName);
        assertEquals(expected, day.part1And2(lines).second());
    }

}