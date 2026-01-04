package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day18Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/18/" + filename);
        var result = Day18.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String filename, int expected) {
        var input = Util.getNotEmptyLinesFromFile("/18/" + filename);
        var result = Day18.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters(){
        return Stream.of(
                Arguments.of("test1.txt", 29),
                Arguments.of("test2.txt", 129),
                Arguments.of("test3.txt", 143),
                Arguments.of("test4.txt", 1384),
                Arguments.of("test5.txt", 445),
                Arguments.of("test6.txt", 791),
                Arguments.of("test7.txt", 1137),
                Arguments.of("test8.txt", 3488),
                Arguments.of("test9.txt", 4140)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("test9.txt", 3993)
        );
    }
}
