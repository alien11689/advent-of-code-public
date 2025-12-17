package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day06Test {

    @ParameterizedTest
    @MethodSource("part1parameters")
    void testPart1Examples(List<String> input, long expected) {
        Object res = Day06.part1(input);
        long val = ((Number) res).longValue();
        assertEquals(expected, val);
    }

    @ParameterizedTest
    @MethodSource("part2parameters")
    void testPart2Examples(List<String> input, long expected) {
        Object res = Day06.part2(input);
        long val = ((Number) res).longValue();
        assertEquals(expected, val);
    }

    private static Stream<Arguments> part1parameters() {
        return Stream.of(
                Arguments.of(List.of("turn on 0,0 through 999,999"), 1_000_000L),
                Arguments.of(List.of("toggle 0,0 through 999,0"), 1_000L),
                Arguments.of(List.of("turn on 0,0 through 999,999", "turn off 499,499 through 500,500"), 1_000_000L - 4L)
        );
    }

    private static Stream<Arguments> part2parameters() {
        return Stream.of(
                Arguments.of(List.of("turn on 0,0 through 0,0"), 1L),
                Arguments.of(List.of("toggle 0,0 through 999,999"), 2_000_000L)
        );
    }
}
