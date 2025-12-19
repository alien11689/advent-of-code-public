package dpr.aoc2016;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day09Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day09.part1(input);
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day09.part2(input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("ADVENT", 6),
                Arguments.of("A(1x5)BC", 7),
                Arguments.of("(3x3)XYZ", 9),
                Arguments.of("A(2x2)BCD(2x2)EFG", 11),
                Arguments.of("(6x1)(1x3)A", 6),
                Arguments.of("X(8x2)(3x3)ABCY", 18)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("(3x3)XYZ", "XYZXYZXYZ".length()),
                Arguments.of("X(8x2)(3x3)ABCY", "XABCABCABCABCABCABCY".length()),
                Arguments.of("(27x12)(20x12)(13x14)(7x10)(1x12)A", 241920),
                Arguments.of("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 445)
        );
    }
}
