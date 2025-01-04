package dpr.aoc2024;

import dpr.commons.Util;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day15Test {

    private final Day15 day = new Day15();

    @CsvSource({
            "test1.txt,10092",
            "test2.txt,2028",
    })
    @ParameterizedTest
    void part1(String fileName, int expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), fileName);
        assertEquals(expected, day.part1(lines));
    }

    @CsvSource({
            "test1.txt,9021",
            "test2.txt,1751",
            "test3.txt,618",
    })
    @ParameterizedTest
    void part2(String fileName, int expected) {
        var lines = Util.getNotEmptyLinesFromFile(day.dayNum(), fileName);
        assertEquals(expected, day.part2(lines));
    }

}