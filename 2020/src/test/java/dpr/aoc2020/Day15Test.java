package dpr.aoc2020;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day15Test {
    @ParameterizedTest
    @MethodSource("part1And2Parameters")
    void testPart1And2(String input, int expectedPart1, int expectedPart2) {
        var result = Day15.part1And2(input);
        assertEquals(2, result.size());
        assertEquals(expectedPart1, result.get(0));
        assertEquals(expectedPart2, result.get(1));
    }

    private static Stream<Arguments> part1And2Parameters() {
        return Stream.of(
                Arguments.of("0,3,6", 436, 175594),
                Arguments.of("1,3,2", 1, 2578),
                Arguments.of("2,1,3", 10, 3544142),
                Arguments.of("1,2,3", 27, 261214),
                Arguments.of("2,3,1", 78, 6895259),
                Arguments.of("3,2,1", 438, 18),
                Arguments.of("3,1,2", 1836, 362)
        );
    }
}
