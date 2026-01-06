package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import dpr.commons.Util;

class Day11Test {
    @ParameterizedTest
    @CsvSource({
            "10,1030",
            "100,8410",
    })
    void testPart1(int expandFactor, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/11/test1.txt");
        var result = Day11.part1And2(input, expandFactor);
        assertEquals(374, result.getFirst());
        assertEquals(expected, result.getSecond());
    }
}
