package dpr.aoc2023;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import dpr.commons.Util;

class Day17Test {
    @Test
    void testPart1() {
        var input = Util.getNotEmptyLinesFromFile("/17/test1.txt");
        var inputSplit = Day17.readBoard(input);
        var result = Day17.part1(inputSplit.component1(), inputSplit.component2());
        assertEquals(102, result);
    }

    @ParameterizedTest
    @CsvSource({
            "test1.txt,94",
            "test2.txt,71",
    })
    void testPart2(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/17/" + filename);
        var inputSplit = Day17.readBoard(input);
        var result = Day17.part2(inputSplit.component1(), inputSplit.component2());
        assertEquals(expected, result);
    }
}
