import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> lines = readInputAsLines('input.txt')
lines = readInputAsLines('other.txt')

@Canonical
class Clay {
    int fromX
    int toX
    int fromY
    int toY

    boolean contains(x, y) {
        x >= fromX && x <= toX && y >= fromY && y <= toY
    }
}

List<Clay> clays = lines.collect { line ->
    String[] part = line.split('[, =]+')
    int fromX
    int toX
    int fromY
    int toY
    int[] corrds1 = part[1].split("\\.\\.").collect { it as int }
    int[] corrds2 = part[3].split("\\.\\.").collect { it as int }
    if (part[0] == 'x') {
        fromX = corrds1[0]
        toX = corrds1.size() > 1 ? corrds1[1] : corrds1[0]
        fromY = corrds2[0]
        toY = corrds2.size() > 1 ? corrds2[1] : corrds2[0]
    } else {
        fromY = corrds1[0]
        toY = corrds1.size() > 1 ? corrds1[1] : corrds1[0]
        fromX = corrds2[0]
        toX = corrds2.size() > 1 ? corrds2[1] : corrds2[0]
    }
    new Clay(fromX, toX, fromY, toY)
}

Set<List<Integer>> claysSet = [] as Set
for (int j = clays.fromY.min(); j <= clays.toY.max(); ++j) {
    for (int i = clays.fromX.min(); i <= clays.toX.max(); ++i) {
        if (clays.any { it.contains(i, j) }) {
            claysSet << [i, j]
        }
    }
}

def printClays(List<Clay> clays, Set<List<Integer>> claysSet, Set<List<Integer>> water) {
    for (int j = clays.fromY.min(); j <= clays.toY.max(); ++j) {
        for (int i = clays.fromX.min(); i <= clays.toX.max(); ++i) {
            if (water.contains([i, j])) {
                print('~')
            } else if (isClay(claysSet, i, j)) {
                print('#')
            } else {
                print('.')
            }
        }
        println()
    }
}

boolean isClay(claysSet, x, y) {
    claysSet.contains([x, y])
}

boolean isFilled(filled, x, y) {
    filled.contains([x, y])
}

enum ActionType {
    GoUp,
    LeakLeft,
    LeakRight
}

class Action {
    int curX
    ActionType type
}

int maxCount = 0

def goDown(claysSet, filled, x, y) {
    int maxY = claysSet.collect {it[1]}.max()
    while (!isClay(claysSet, x, y + 1) && y + 1 <= maxY) {
        ++y
        filled << [x, y]
    }
    //try to fill
    boolean boundLeft = fillLeft(x, claysSet, y, filled)
    boolean boundRight = fillRight(x, claysSet, y, filled)
    while (boundLeft && boundRight) {
        --y
        boundLeft = fillLeft(x, claysSet, y, filled)
        boundRight = fillRight(x, claysSet, y, filled)
    }
    if (boundLeft) {
//        TODO find nearest right
        int cur = x + 1
        while (isClay(claysSet, cur, y + 1) || isFilled(filled, cur, y + 1)) {
            ++cur
        }
        println("Going donw on $cur")
    }

    if (boundRight) {
//        TODO find nearest right
        int cur = x - 1
        while (isClay(claysSet, cur, y + 1) || isFilled(filled, cur, y + 1)) {
            ++cur
        }
        println("Going donw on $cur")
    }
}

for (int x = clays.fromX.min(); x <= clays.toX.max(); ++x) {
    Set<List<Integer>> filled = [] as Set
    int y = clays.fromY.min() - 1
    goDown(claysSet, filled, x, y)
    printClays(clays, claysSet, filled)
    if (filled.size() > maxCount) {
        maxCount = filled.size()
    }
    println('------------')
}
println(maxCount)

private boolean fillRight(int x, Set claysSet, int y, Set filled) {
    println("Filling right")
    boolean boundRight = false
    int cur = x
    while (true) {
//        println(filled)
        int nextX = cur + 1
        if (isClay(claysSet, nextX, y)) {
            println("Next is clay")
            boundRight = true
            break
        }
        filled << [nextX, y]
        if (!isClay(claysSet, nextX, y + 1) && !filled.contains([nextX, y + 1])) {
            println("Falling down")
            // TODO fall down
            break
        }
        ++cur
    }
    boundRight
}

private boolean fillLeft(int x, Set claysSet, int y, Set filled) {
    println("Filling left")
    int cur = x
    boolean boundLeft = false
    while (true) {
//        println(filled)
        int prevX = cur - 1
        if (isClay(claysSet, prevX, y)) {
            println("Prev is clay")
            boundLeft = true
            break
        }
        filled << [prevX, y]
        if (!isClay(claysSet, prevX, y + 1) && !filled.contains([prevX, y + 1])) {
            println('Falling down')
            // TODO fall down
            break
        }
        --cur
    }
    boundLeft
}