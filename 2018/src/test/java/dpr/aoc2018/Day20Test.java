
package dpr.aoc2018;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import dpr.commons.Util;

class Day20Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day20.part1And2(input);
        assertEquals(expected, result.getFirst());
        assertEquals(0, result.getSecond());
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("^WNE$", 3),
                Arguments.of("^ENWWW(NEEE|SSE(EE|N))$", 10),
                Arguments.of("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$", 18),
                Arguments.of("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$", 23),
                Arguments.of("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$", 31)
        );
    }
}
