package dpr.aoc2024;

import dpr.commons.Util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

class Day22 implements Day {
    public static void main(String... args) {
        new Day22().execute(args);
    }

    @Override
    public void execute(String... args) {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/input.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test1.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test2.txt", dayNum()));
//            var lines = Util.getNotEmptyLinesFromFile(String.format("/%02d/test3.txt", dayNum()));
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 22;
    }

    private Object part1(List<String> lines) {
        long sum = 0;
        for (String line : lines) {
            long init = Long.parseLong(line);
            long secret = calculatePseudo(init, 2000);
            sum += secret;
        }
        return sum;
    }

    private long calculatePseudo(long init, int iter) {
        long cur = init;
        for (int i = 1; i <= iter; i++) {
            cur = (cur ^ (cur << 6)) % 16777216;
            cur = (cur ^ (cur >> 5)) % 16777216;
            cur = (cur ^ (cur << 11)) % 16777216;
        }
        return cur;
    }

    private Object part2(List<String> lines) {
        Set<String> keys = new HashSet<>();
        List<Map<String, Integer>> pricesList = new ArrayList<>();
        for (String line : lines) {
            long init = Long.parseLong(line);
            Map<String, Integer> prices = calculatePseudo2(init, 2000);
            pricesList.add(prices);
            keys.addAll(prices.keySet());
        }
        long best = Long.MIN_VALUE;
        for (String key : keys) {
            long sum = pricesList.stream().mapToLong(prices -> prices.getOrDefault(key, 0)).sum();
            if (sum > best) {
                best = sum;
            }
        }
        return best;
    }

    private Map<String, Integer> calculatePseudo2(long init, int iter) {
        long cur = init;
        long last = cur % 10;
        BlockingQueue<Long> q = new ArrayBlockingQueue<>(4);
        Map<String, Integer> prices = new HashMap<>();
        for (int i = 1; i <= iter; i++) {
            cur = (cur ^ (cur << 6)) % 16777216;
            cur = (cur ^ (cur >> 5)) % 16777216;
            cur = (cur ^ (cur << 11)) % 16777216;
            long newlast = cur % 10;
            if (q.size() == 4) {
                q.poll();
            }
            q.add(newlast - last);
            String key = q.toString();
            if (!prices.containsKey(key)) {
                prices.put(key, (int) newlast);
            }
//            System.out.println(init + " in iteration " + i + " " + key + " -> " + newlast);
            last = newlast;
        }
        return prices;
    }
}
