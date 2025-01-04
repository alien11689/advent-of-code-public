package dpr.aoc2024;

import dpr.commons.Day;
import dpr.commons.Util;

import java.util.List;

class Day13 implements Day {
    public static void main(String... args) {
        new Day13().execute();
    }

    @Override
    public void execute() {
        Util.measureTime(() -> {
            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
//            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "test1.txt");
//            var lines = Util.getNotEmptyLinesFromFile(dayNum(), "test2.txt");
            System.out.println(part1(lines));
            System.out.println(part2(lines));
        });
    }

    @Override
    public int dayNum() {
        return 13;
    }

    Object part1(List<String> lines) {
        return calculateWithBisect(lines, 0L);
    }

    Object part2(List<String> lines) {
        return calculateWithBisect(lines, 10000000000000L);
    }

    private static long calculateWithBisect(List<String> lines, long resultIncrement) {
        long res = 0;
        for (int i = 0; i < lines.size(); i += 3) {
            String[] split1 = lines.get(i).split("[+,]");
            int ax = Integer.parseInt(split1[1]);
            int ay = Integer.parseInt(split1[3]);
            String[] split2 = lines.get(i + 1).split("[+,]");
            int bx = Integer.parseInt(split2[1]);
            int by = Integer.parseInt(split2[3]);
            String[] split3 = lines.get(i + 2).split("[=,]");
            long cx = Integer.parseInt(split3[1]) + resultIncrement;
            long cy = Integer.parseInt(split3[3]) + resultIncrement;
            long partialResult = 0L;
//            System.out.println("ax=" + ax + ", ay=" + ay + ", " + "bx=" + bx + ", by=" + by + ", " + "cx=" + cx + ", cy=" + cy);
            long bmax = Math.min(cx / bx, cy / by);
            long bmin = 0;
            while (bmin <= bmax) {
                double adiffForBmin = calculateAdiff(cx, bmin, bx, cy, by, ax, ay);
                double adiffForBMax = calculateAdiff(cx, bmax, bx, cy, by, ax, ay);
                if (Math.signum(adiffForBMax) != 0 && Math.signum(adiffForBMax) == Math.signum(adiffForBmin) && Math.signum(adiffForBmin) != 0) {
                    break;
                }
                long b = (bmax + bmin) / 2;
                // ax * a + bx * b = cx
                // ay * a + by * b = cy
                long resX = cx - b * bx;
                long resY = cy - b * by;
                double a1 = 1.0 * resX / ax;
                double a2 = 1.0 * resY / ay;
                double aDiff = a1 - a2;
                long la1 = resX / ax;
                long la2 = resY / ay;
//                System.out.printf("For b=%s (%s, %s), a1 = %s, a2 = %s, aDiff = %s, adiffRange (%s, %s) \n", b, bmin, bmax, a1, a2, aDiff, adiffForBmin, adiffForBMax);
                if (la1 >= 0 && la1 == la2 && la1 * ax == resX && la2 * ay == resY) {
//                    System.out.println("For b = " + b + " possible a = " + la1 + ", adiff = " + aDiff);
                    partialResult = la1 * 3 + b;
                    break;
                }
                if (adiffForBmin < adiffForBMax) {
                    if (aDiff > 0) {
                        bmax = b - 1;
                    } else {
                        bmin = b + 1;
                    }
                } else {
                    if (aDiff < 0) {
                        bmax = b - 1;
                    } else {
                        bmin = b + 1;
                    }
                }
            }
            res += partialResult;
        }
        return res;
    }

    private static double calculateAdiff(long cx, long b, int bx, long cy, int by, int ax, int ay) {
        long resX = cx - b * bx;
        long resY = cy - b * by;
        double a1 = 1.0 * resX / ax;
        double a2 = 1.0 * resY / ay;
        return a1 - a2;
    }
}
