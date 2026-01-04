package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day22Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, long expected) {
        var input = Util.getNotEmptyLinesFromFile("/22/" + filename);
        var result = Day22.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, long expected) {
        var input = Util.getNotEmptyLinesFromFile("/22/" + filename);
        var result = Day22.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters(){
        return Stream.of(
                Arguments.of("test1.txt", 39),
                Arguments.of("test2.txt", 590784),
                Arguments.of("test3.txt", 474140)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test3.txt", 2758514936282235L)
        );
    }
}
