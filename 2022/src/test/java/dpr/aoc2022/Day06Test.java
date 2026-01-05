package dpr.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day06Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day06.part1(List.of(input));
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day06.part2(List.of(input));
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
                Arguments.of("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
                Arguments.of("nppdvjthqldpwncqszvftbrmjlhg", 6),
                Arguments.of("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
                Arguments.of("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
                Arguments.of("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
                Arguments.of("nppdvjthqldpwncqszvftbrmjlhg", 23),
                Arguments.of("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
                Arguments.of("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)
        );
    }
}
