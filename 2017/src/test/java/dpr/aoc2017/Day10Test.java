package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day10Test {
    @Test
    void testPart1() {
        var result = Day10.part1("3,4,1,5", 4);
        assertEquals(12, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, String expected) {
        var result = Day10.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("", "a2582a3a0e66e6e86e3812dcb672a272"),
                Arguments.of("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
                Arguments.of("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d"),
                Arguments.of("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
        );
    }
}
