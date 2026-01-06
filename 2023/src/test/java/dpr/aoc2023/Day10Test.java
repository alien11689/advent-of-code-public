package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import dpr.commons.Util;

class Day10Test {
    @ParameterizedTest
    @CsvSource({
            "test1.txt,4",
            "test2.txt,8"
    } )
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/10/" + filename);
        var result = Day10.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @CsvSource({
            "test3.txt,8",
            "test4.txt,10",
    } )
    void testPart2(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/10/" + filename);
        var result = Day10.part2(input);
        assertEquals(expected, result);
    }
}
