package dpr.aoc2021;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class Day16Test {
    @ParameterizedTest
    @MethodSource("part1Parameters")
    void testPart1(String input, int expected) {
        var result = Day16.part1(List.of(input));
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @MethodSource("part2Parameters")
    void testPart2(String input, int expected) {
        var result = Day16.part2(List.of(input));
        assertEquals(expected, result);
    }

    private static Stream<Arguments> part1Parameters() {
        return Stream.of(
                Arguments.of("D2FE28", 6),
                Arguments.of("38006F45291200", 9),
                Arguments.of("EE00D40C823060", 14),
                Arguments.of("8A004A801A8002F478", 16),
                Arguments.of("620080001611562C8802118E34", 12),
                Arguments.of("C0015000016115A2E0802F182340", 23),
                Arguments.of("A0016C880162017C3686B18A3D4780", 31)
        );
    }

    private static Stream<Arguments> part2Parameters() {
        return Stream.of(
                Arguments.of("C200B40A82", 3),
                Arguments.of("04005AC33890", 54),
                Arguments.of("880086C3E88112", 7),
                Arguments.of("CE00C43D881120", 9),
                Arguments.of("D8005AC2A8F0", 1),
                Arguments.of("F600BC2D8F", 0),
                Arguments.of("9C005AC2F8F0", 0),
                Arguments.of("9C0141080250320F1802104A08", 1)
        );
    }
}
