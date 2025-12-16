package dpr.aoc2015;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day05Test {

    @ParameterizedTest
    @MethodSource("part1parameters")
    void testPart1(String input, boolean expected) {
        var result = Day05.part1(java.util.Collections.singletonList(input));
        boolean isNice = ((Number) result).intValue() == 1;
        assertEquals(expected, isNice);
    }

    @ParameterizedTest
    @MethodSource("part2parameters")
    void testPart2(String input, boolean expected) {
        Object result = Day05.part2(java.util.Collections.singletonList(input));
        boolean isNice = ((Number) result).intValue() == 1;
        assertEquals(expected, isNice);
    }

    private static Stream<Arguments> part1parameters() {
        return Stream.of(
                Arguments.of("ugknbfddgicrmopn", true),
                Arguments.of("aaa", true),
                Arguments.of("jchzalrnumimnmhp", false),
                Arguments.of("haegwjzuvuyypxyu", false),
                Arguments.of("dvszwmarrgswjxmb", false)
        );
    }

    private static Stream<Arguments> part2parameters() {
        return Stream.of(
                Arguments.of("qjhvhtzxzqqjkmpb", true),
                Arguments.of("xxyxx", true),
                Arguments.of("uurcxstgmygtbstg", false),
                Arguments.of("ieodomkazucvgmuy", false)
        );
    }
}
