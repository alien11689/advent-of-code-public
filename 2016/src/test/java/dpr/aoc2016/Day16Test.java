package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day16Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, String expected, int size) {
        var result = Day16.part1(input, size);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("110010110100", "100", 12),
                Arguments.of("10000", "01100", 20)
        );
    }
}
