package dpr.aoc2017;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Map;

import org.junit.jupiter.api.Test;

import dpr.commons.Pair;

class Day25Test {
    @Test
    void testPart1() {
        var turingMachine = new Day25.TuringMachine(
                'A', 6, Map.of(
                new Pair<>('A', 0), new Day25.Transition(1, 1, 'B'),
                new Pair<>('A', 1), new Day25.Transition(0, -1, 'B'),
                new Pair<>('B', 0), new Day25.Transition(1, -1, 'A'),
                new Pair<>('B', 1), new Day25.Transition(1, 1, 'A')
        )
        );
        var result = Day25.part1(turingMachine);
        assertEquals(3, result);
    }
}
