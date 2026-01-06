package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import dpr.commons.Util;

class Day20Test {
    @ParameterizedTest
    @CsvSource({
            "test1.txt,32000000",
            "test2.txt,11687500",
    })
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/20/" + filename);
        var result = Day20.part1(input);
        assertEquals(expected, result);
    }
}
