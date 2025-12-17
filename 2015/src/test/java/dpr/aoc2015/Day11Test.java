package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day11Test {

    @ParameterizedTest
    @MethodSource("parameters")
    void testNextPasswords(String input, String expected) {
        var res1 = Day11.part1And2(input, 1);
        assertEquals(expected, res1.getFirst());
    }

    private static Stream<Arguments> parameters() {
        return Stream.of(
                Arguments.of("abcdefgh", "abcdffaa"),
                Arguments.of("ghijklmn", "ghjaabcc")
        );
    }
}
