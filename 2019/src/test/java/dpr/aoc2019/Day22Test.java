package dpr.aoc2019;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day22Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, String expectedAsString) {
        var input = Util.getNotEmptyLinesFromFile("/22/" + filename);
        var results = new HashMap<Long, Integer>();
        for (int i = 0; i < 10; ++i) {
            var result = Day22.part1(input, 10, i);
            results.put(result, i);
        }
        var resultAsString = results.entrySet().stream().sorted(Map.Entry.comparingByKey()).map(Map.Entry::getValue).map(Objects::toString).collect(Collectors.joining(" "));
        assertEquals(expectedAsString, resultAsString);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", "0 3 6 9 2 5 8 1 4 7"),
                Arguments.of("test2.txt", "3 0 7 4 1 8 5 2 9 6"),
                Arguments.of("test3.txt", "6 3 0 7 4 1 8 5 2 9"),
                Arguments.of("test4.txt", "9 2 5 8 1 4 7 0 3 6")
        );
    }
}
