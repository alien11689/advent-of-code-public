package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day12Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/12/" + filename);
        var result = Day12.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/12/" + filename);
        var result = Day12.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters(){
        return Stream.of(
                Arguments.of("test1.txt", 10),
                Arguments.of("test2.txt", 19),
                Arguments.of("test3.txt", 226)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test1.txt", 36),
                Arguments.of("test2.txt", 103),
                Arguments.of("test3.txt", 3509)
        );
    }
}
