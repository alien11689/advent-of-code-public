package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day12Test {

    private final Day12 day = new Day12();

    @CsvSource({
            "test1.txt,140",
            "test2.txt,772",
            "test3.txt,1930",
    })
    @ParameterizedTest
    void part1(String fileName, long expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), fileName);
        assertEquals(expected, day.part1(lines));
    }

    @CsvSource({
            "test1.txt,80",
            "test2.txt,436",
            "test3.txt,1206",
            "test4.txt,236",
            "test5.txt,368",
    })
    @ParameterizedTest
    void part2(String fileName, long expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), fileName);
        assertEquals(expected, day.part2(lines));
    }

}