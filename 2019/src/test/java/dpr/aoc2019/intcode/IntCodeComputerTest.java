package dpr.aoc2019.intcode;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

public class IntCodeComputerTest {

    @ParameterizedTest
    @MethodSource("day5Parameters")
    void testRun(String program, Collection<Long> input, List<Long> expected) {
        var output = IntCodeComputer.run(program, input).stream().toList();
        assertEquals(expected, output);
    }

    private static Stream<Arguments> day5Parameters() {
        return Stream.of(
                Arguments.of("3,9,8,9,10,9,4,9,99,-1,8", List.of(7L), List.of(0L)),
                Arguments.of("3,9,8,9,10,9,4,9,99,-1,8", List.of(8L), List.of(1L)),
                Arguments.of("3,9,8,9,10,9,4,9,99,-1,8", List.of(9L), List.of(0L)),
                Arguments.of("3,9,7,9,10,9,4,9,99,-1,8", List.of(7L), List.of(1L)),
                Arguments.of("3,9,7,9,10,9,4,9,99,-1,8", List.of(8L), List.of(0L)),
                Arguments.of("3,9,7,9,10,9,4,9,99,-1,8", List.of(9L), List.of(0L)),
                Arguments.of("3,3,1108,-1,8,3,4,3,99", List.of(7L), List.of(0L)),
                Arguments.of("3,3,1108,-1,8,3,4,3,99", List.of(8L), List.of(1L)),
                Arguments.of("3,3,1108,-1,8,3,4,3,99", List.of(9L), List.of(0L)),
                Arguments.of("3,3,1107,-1,8,3,4,3,99", List.of(7L), List.of(1L)),
                Arguments.of("3,3,1107,-1,8,3,4,3,99", List.of(8L), List.of(0L)),
                Arguments.of("3,3,1107,-1,8,3,4,3,99", List.of(9L), List.of(0L)),
                Arguments.of("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", List.of(0L), List.of(0L)),
                Arguments.of("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", List.of(1L), List.of(1L)),
                Arguments.of("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", List.of(2L), List.of(1L)),
                Arguments.of("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", List.of(0L), List.of(0L)),
                Arguments.of("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", List.of(1L), List.of(1L)),
                Arguments.of("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", List.of(2L), List.of(1L)),
                Arguments.of("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," +
                        "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," +
                        "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", List.of(7L), List.of(999L)),
                Arguments.of("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," +
                        "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," +
                        "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", List.of(8L), List.of(1000L)),
                Arguments.of("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," +
                        "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," +
                        "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", List.of(9L), List.of(1001L))
        );
    }
}
