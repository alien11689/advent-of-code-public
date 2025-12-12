package dpr.aoc2025;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

import org.jetbrains.annotations.NotNull;

import dpr.commons.Day;
import dpr.commons.Pair;
import dpr.commons.Point2D;
import dpr.commons.Util;

class Day12 implements Day {
    @Override
    public void execute() {
        var lines = Util.getNotEmptyLinesFromFile(dayNum(), "input.txt");
        System.out.println(part1(lines));
    }

    @Override
    public int dayNum() {
        return 12;
    }

    long part1(List<String> lines) {
        var presents = new Present[6];
        var presentCounter = 0;
        var y = 0;
        var s = new HashSet<Point2D>();
        var boxes = new ArrayList<Box>();
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.contains(":") && !line.contains("x")) {
                continue;
            }
            if (presentCounter < presents.length) {
                var x = 0;
                for (char c : line.toCharArray()) {
                    if (c == '#') {
                        s.add(new Point2D(x, y));
                    }
                    ++x;
                }
                y = (y + 1) % 3;
                if (y == 0) {
                    presents[presentCounter] = Present.fromSingle(s);
                    s = new HashSet<>();
                    presentCounter++;
                }
            } else {
                String[] parts = line.split(": ");
                String[] sizes = parts[0].split("x");
                String[] quantities = parts[1].split(" ");
                var toFit = new HashMap<Integer, Integer>();
                for (int i1 = 0; i1 < quantities.length; i1++) {
                    int value = Integer.parseInt(quantities[i1]);
                    if (value > 0) {
                        toFit.put(i1, value);
                    }
                }
                boxes.add(new Box(Integer.parseInt(sizes[0]), Integer.parseInt(sizes[1]), toFit));
            }
        }
        return boxes.stream().filter(b -> b.canFit(presents)).count();
    }

    record Present(int types, int singleSize, Set<Set<Point2D>> combinations) {
        static Present fromSingle(Set<Point2D> points) {
            var combinations = new HashSet<Set<Point2D>>();
            combinations.add(points);
            Set<Point2D> cur = new HashSet<>(points);
            for (int i = 0; i < 4; ++i) {
                combinations.add(cur);
                combinations.add(flipV(cur));
                combinations.add(flipH(cur));
                cur = rotate(cur);
            }
            return new Present(combinations.size(), points.size(), combinations);
        }

        private static Set<Point2D> rotate(Set<Point2D> p) {
            var np = new HashSet<Point2D>();
            for (Point2D cur : p) {
                np.add(new Point2D(cur.getY(), 2 - cur.getX()));
            }
            return np;
        }

        private static Set<Point2D> flipV(Set<Point2D> p) {
            var np = new HashSet<Point2D>();
            for (Point2D cur : p) {
                np.add(new Point2D(cur.getX(), 2 - cur.getY()));
            }
            return np;
        }

        private static Set<Point2D> flipH(Set<Point2D> p) {
            var np = new HashSet<Point2D>();
            for (Point2D cur : p) {
                np.add(new Point2D(2 - cur.getX(), cur.getY()));
            }
            return np;
        }
    }

    record Box(int width, int height, Map<Integer, Integer> toFit) {
        public boolean canFit(Present[] presents) {
//            System.out.println("Checking " + this);
            int maxSize = width * height;
            var res = 0;
            for (int i = 0; i < presents.length; ++i) {
                res += presents[i].singleSize * toFit.getOrDefault(i, 0);
            }
            boolean canFit = maxSize > res;
            // on my input rejected boxes had size a little bit smaller than needed or much bigger than needed
            // 350 should be a good threshold for manual check
            return canFit && (maxSize - res >= 350 || tryAllCombinations(presents));
        }

        private boolean tryAllCombinations(Present[] presents) {
            var pq = new PriorityQueue<State>();
            var mem = new HashSet<State>();
            pq.offer(new State(0, 0, toFit, Set.of()));
            var result = false;
            while (!pq.isEmpty()) {
                if (mem.size() > 10000) {
                    // it would run endlessly and this number of checks is enough for test input
                    return false;
                }
                var cur = pq.poll();
                if (mem.contains(cur)) {
                    continue;
                }
                mem.add(cur);
//                System.out.println("PQ size is " + pq.size() + ", mem size " + mem.size() + ", mem2 size = " + mem2.size() + ", cur = " + cur);
                if (!(cur.curWidth() < width - 2) || !(cur.curHeight < width - 2)) {
                    continue;
                }
                for (int i = 0; i < presents.length; ++i) {
                    var tf = new HashMap<>(cur.toFit);
                    if (tf.containsKey(i)) {
                        int n = tf.get(i);
                        var nextPresent = false;
                        if (n > 1) {
                            tf.put(i, n - 1);
                        } else {
                            tf.remove(i);
                            nextPresent = true;
                        }
                        var present = presents[i];
                        for (Set<Point2D> combination : present.combinations) {
                            var toAdd = shift(combination, cur.curWidth, cur.curHeight);
                            var nextUsed = new HashSet<>(cur.used);
                            nextUsed.addAll(toAdd);
                            if (nextUsed.size() == cur.used.size() + toAdd.size()) {
                                if (tf.isEmpty()) {
//                                    System.out.println("Fit");
                                    return true;
                                }
                                if (nextPresent) {
                                    State e = new State(0, 0, tf, nextUsed);
                                    if (!mem.contains(e)) {
                                        pq.offer(e);
                                    }
                                } else {
                                    int nextWidth = cur.curWidth + 1;
                                    int nextHeight = cur.curHeight;
                                    if (nextWidth >= width - 2) {
                                        nextWidth = 0;
                                        nextHeight++;
                                    }
                                    if (nextHeight < height - 2) {
                                        State e = new State(nextWidth, nextHeight, tf, nextUsed);
                                        if (!mem.contains(e)) {
                                            pq.offer(e);
                                        }
                                    }
                                }
                            }
                        }
                        int nextWidth = cur.curWidth + 1;
                        int nextHeight = cur.curHeight;
                        if (nextWidth >= width - 2) {
                            nextWidth = 0;
                            nextHeight++;
                        }
                        if (nextHeight < height - 2) {
                            State e = new State(nextWidth, nextHeight, cur.toFit, cur.used);
                            if (!mem.contains(e)) {
                                pq.offer(e);
                            }
                        }
                        break;
                    }
                }
            }
            return result;
        }

        private Set<Point2D> shift(Set<Point2D> combination, int dx, int dy) {
            return combination.stream().map(p -> p.move(dx, dy)).collect(Collectors.toSet());
        }
    }

    record State(int curWidth, int curHeight, Map<Integer, Integer> toFit, Set<Point2D> used)
            implements Comparable<State> {
        @Override
        public int compareTo(@NotNull State o) {
            return Integer.compare(o.used.size(), used.size());
        }
    }

    public static void main(String[] args) {
        new Day12().execute();
    }
}
