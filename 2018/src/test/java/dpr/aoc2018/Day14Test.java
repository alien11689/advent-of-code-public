
package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day14Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(int input, String expected) {
        var result = Day14.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day14.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of(9, "5158916779"),
                Arguments.of(5, "0124515891"),
                Arguments.of(18, "9251071085"),
                Arguments.of(2018, "5941429882")
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("51589", 9),
                Arguments.of("01245", 5),
                Arguments.of("92510", 18),
                Arguments.of("59414", 2018)
        );
    }
}
