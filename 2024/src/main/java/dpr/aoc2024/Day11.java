package dpr.aoc2024;

import dpr.commons.Util;

import java.util.Arrays;
import java.util.List;
import java.util.Stack;

class Day11 implements Day {
    public static void main(String... args) {
        new Day11().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
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

    private Object part1(List<String> lines) {
        Stack<Item> q = new Stack<>();
        Arrays.stream(lines.get(0).split(" ")).forEach(s -> q.push(new Item(Long.parseLong(s), 0)));
        return countItems(q, 25);
    }

    private static long countItems(Stack<Item> q, int max) {
        long count = 0;
        while (!q.isEmpty()) {
            Item cur = q.pop();
            for (Item item : cur.next()) {
                if (item.iteration == max) {
                    ++count;
                } else {
                    q.push(item);
                }
            }
        }
        return count;
    }

    private Object part2(List<String> lines) {
        Stack<Item> q = new Stack<>();
        Arrays.stream(lines.get(0).split(" ")).forEach(s -> q.push(new Item(Long.parseLong(s), 0)));
        return countItems(q, 75);
    }
}
