package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day17Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, String expected) {
        var result = Day17.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day17.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("ihgpwlah", "DDRRRD"),
                Arguments.of("kglvqrro", "DDUDRLRRUDRD"),
                Arguments.of("ulqzkmiv", "DRURDRUDDLLDLUURRDULRLDUUDDDRR")
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("ihgpwlah", 370),
                Arguments.of("kglvqrro", 492),
                Arguments.of("ulqzkmiv", 830)
        );
    }
}
