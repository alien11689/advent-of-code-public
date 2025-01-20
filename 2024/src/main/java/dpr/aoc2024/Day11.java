package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Util;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Day11 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
        System.out.println(part2(lines));
    }

    @Override
    public int dayNum() {
        return 11;
    }

    record Item(long num, int iteration) {
        public List<Item> next() {
            if (num < 0) {
                throw new RuntimeException("Overflow " + num);
            }
            if (num == 0) {
                return List.of(new Item(1, iteration + 1));
            }
            int digits = (int) Math.log10(num) + 1;
            if (digits % 2 == 0) {
                int pow = (int) Math.pow(10, digits / 2);
                return List.of(new Item(num / pow, iteration + 1), new Item(num % pow, iteration + 1));
            } else {
                return List.of(new Item(num * 2024, iteration + 1));
            }
        }
    }

    Object part1(List<String> lines) {
        return calculate(lines, 25);
    }

    private Object part2(List<String> lines) {
        return calculate(lines, 75);
    }

    private static long calculate(List<String> lines, int max) {
        Map<Pair<Long, Integer>, Long> memory = new HashMap<>();
        return Arrays.stream(lines.getFirst().split(" "))
                .mapToLong(s -> countItems(new Item(Long.parseLong(s), 0), max, memory))
                .sum();
    }

    private static long countItems(Item item, int max, Map<Pair<Long, Integer>, Long> memory) {
        if (item.iteration == max) {
            return 1;
        } else {
            Pair<Long, Integer> key = new Pair<>(item.num, item.iteration);
            if (memory.containsKey(key)) {
//                System.out.println("Cached used for key " + key);
                return memory.get(key);
            }
            long sum = item.next().stream().mapToLong(i -> countItems(i, max, memory)).sum();
            memory.put(key, sum);
            return sum;
        }
    }
}
