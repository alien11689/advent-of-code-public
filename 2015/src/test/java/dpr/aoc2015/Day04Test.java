package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day04Test {

    @ParameterizedTest
    @MethodSource("cases")
    void testPart1And2(String input, String prefix, int init, int expected) {
        assertEquals(expected, Day04.part1And2(input, prefix, init));
    }

    private static Stream<Arguments> cases() {
        return Stream.of(
                Arguments.of("abcdef", "00000", 0, 609043),
                Arguments.of("pqrstuv", "00000", 0, 1048970),
                Arguments.of("abcdef", "000000", 609043, 6742839),
                Arguments.of("pqrstuv", "000000", 1048970, 5714438)
        );
    }
}
