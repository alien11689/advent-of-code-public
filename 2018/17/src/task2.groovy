import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> lines = readInputAsLines('input.txt')
//lines = readInputAsLines('other.txt')

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

def printClays(List<Clay> clays, Set<List<Integer>> claysSet, Set<List<Integer>> water, Set<List<Integer>> drained) {
    for (int j = clays.fromY.min(); j <= clays.toY.max(); ++j) {
        for (int i = clays.fromX.min() - 1; i <= clays.toX.max() + 1; ++i) {
            if (drained.contains([i, j])) {
                print('~')
            } else if (water.contains([i, j])) {
                print('|')
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
    Leak,
    Nop
}

@Canonical
class Action {
    ActionType type
    int curX
}

int maxCount = 0

goDown = [] as Set

def goDown(claysSet, filled, x, y, drained) {
    goDown << [x, y]
//    println("Cur [$x,$y]")
    int maxY = claysSet.collect { it[1] }.max()
    while (!isClay(claysSet, x, y + 1) && y + 1 <= maxY) {
        ++y
        filled << [x, y]
    }
    if (y + 1 > maxY) {
//        println("Reaching end $x")
        return
    }
    Action boundLeft = fillLeft(x, claysSet, y, filled, drained)
    Action boundRight = fillRight(x, claysSet, y, filled, drained)
    while (boundLeft.type == ActionType.GoUp && boundRight.type == ActionType.GoUp) {
        drained << [x, y]
        int i = x - 1
        while (!isClay(claysSet, i, y)) {
            drained << [i, y]
            --i
        }
        i = x + 1
        while (!isClay(claysSet, i, y)) {
            drained << [i, y]
            ++i
        }
        --y
        boundLeft = fillLeft(x, claysSet, y, filled, drained)
        boundRight = fillRight(x, claysSet, y, filled, drained)
    }
    if (boundLeft.type == ActionType.Leak) {
//        println("Leaking left $boundLeft.curX")
        goDown(claysSet, filled, boundLeft.curX, y, drained)
    }
    if (boundRight.type == ActionType.Leak) {
//        println("Leaking right $boundRight.curX")
        goDown(claysSet, filled, boundRight.curX, y, drained)
    }
}

private Action fillRight(int x, Set claysSet, int y, Set filled, drained) {
//    println("Filling right")
    int cur = x
    while (true) {
        int nextX = cur + 1
        if (isClay(claysSet, nextX, y)) {
//            println("Next is clay")
            return new Action(ActionType.GoUp)
        }
        boolean nextIsClay = isClay(claysSet, nextX, y + 1)
        boolean nextIsFilled = filled.contains([nextX, y + 1])
        if (!nextIsClay && !nextIsFilled) {
            if (goDown.contains([nextX + 1, y]) || goDown.contains([nextX - 1, y])) {
                return new Action(ActionType.Nop)
            }
//            println("Falling down on right")
            filled << [nextX, y]
            return new Action(ActionType.Leak, nextX)
//        } else if (nextIsClay && filled.contains([nextX + 1, y + 1])) {
////            println("Falling down on right")
//            return new Action(ActionType.Nop)
        }
        ++cur
        filled << [nextX, y]
        continue
    }
}

private Action fillLeft(int x, Set claysSet, int y, Set filled, Set drained) {
    int cur = x
    while (true) {
        int prevX = cur - 1
        if (isClay(claysSet, prevX, y)) {
//            println("Prev is clay")
            return new Action(ActionType.GoUp)
        }
        boolean prevIsClay = isClay(claysSet, prevX, y + 1)
        boolean prevIsFilled = filled.contains([prevX, y + 1])
        if (!prevIsClay && !prevIsFilled) {
            if (goDown.contains([prevX + 1, y]) || goDown.contains([prevX - 1, y])) {
                return new Action(ActionType.Nop)
            }
            filled << [prevX, y]
//            println('Falling down on left')
            return new Action(ActionType.Leak, prevX)
//        } else if (prevIsClay && filled.contains([prevX - 1, y + 1])) {
////            println("Falling down on right")
//            return new Action(ActionType.Nop)
        }
        --cur
        filled << [prevX, y]
        continue
    }
}


println("Reading clays")
Set<List<Integer>> claysSet = [] as Set
clays.each {
    for (int j = it.fromY; j <= it.toY; ++j) {
        for (int i = it.fromX; i <= it.toX; ++i) {
            claysSet << [i, j]
        }
    }
}

println("Calculating")
//for (int x = clays.fromX.min() - 1; x <= clays.toX.max() + 1; ++x) {
for (int x = 500; x <= 500; ++x) {
    println("Checking $x")
    Set<List<Integer>> filled = [] as Set
    Set<List<Integer>> drained = [] as Set
    int y = clays.fromY.min() - 1
    goDown(claysSet, filled, x, y, drained)
//    println("Filling empty slots between water")
//    for (int i = clays.fromX.min(); i <= clays.toX.max(); ++i) {
//        for (int j = clays.fromY.min(); j <= clays.toY.max(); ++j) {
//            if (isFilled(filled, i - 1, j) && isFilled(filled, i + 1, j) && !isFilled(filled, i, j) && !isClay(claysSet, i, j))
//                filled << [i, j]
//        }
//    }

    printClays(clays, claysSet, filled, drained)
    println("Drained ${drained.size()}")
    if (filled.size() > maxCount) {
        maxCount = filled.size()
    }
    println("Current max is $maxCount")
    println('------------')
}
println(maxCount)