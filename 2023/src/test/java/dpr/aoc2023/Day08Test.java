package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import dpr.commons.Util;

class Day08Test {
    @ParameterizedTest
    @CsvSource({
            "test1.txt,2",
            "test2.txt,6"
    } )
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/08/" + filename);
        var result = Day08.part1(input);
        assertEquals(expected, result);
    }

    @Test
    void testPart2() {
        var input = Util.getNotEmptyLinesFromFile("/08/test3.txt");
        var result = Day08.part2(input);
        assertEquals(6, result);
    }
}
