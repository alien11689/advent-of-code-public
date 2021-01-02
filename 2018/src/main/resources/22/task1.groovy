import groovy.transform.Canonical

depth = 510l
target = new Pos(10, 10)

depth = 11541
target = new Pos(14, 778)

@Canonical
class Pos {
    int x
    int y

    Pos left() {
        return new Pos(x - 1, y)
    }

    Pos up() {
        return new Pos(x, y - 1)
    }
}

pos2Erosion = [:] // Pos -> Long
geoIndex = [:] // Pos -> Long

long geologicalIndex(Pos pos) {
    if (geoIndex.keySet().contains(pos)) {
        return geoIndex[pos]
    }
    if (pos.x == 0 && pos.y == 0) {
        geoIndex[pos] = 0
        return 0
    }
    if (target.x == pos.x && target.y == pos.y) {
        geoIndex[pos] = 0
        return 0
    }
    if (pos.y == 0) {
        def value = pos.x * 16807l
        geoIndex[pos] = value
//        println("GI: $pos -> $value")
        return value
    }
    if (pos.x == 0) {
        def value = pos.y * 48271l
        geoIndex[pos] = value
//        println("GI: $pos -> $value")
        return value
    }
    def value = pos2Erosion[pos.left()] * pos2Erosion[pos.up()]
    geoIndex[pos] = value
//    println("GI: $pos -> $value")
    return value
}

int erosionLevel(Pos pos) {
    if (pos.x < 0 || pos.y < 0) {
        return 0
    }
    if (pos2Erosion.keySet().contains(pos)) {
        return pos2Erosion[pos]
    }
    long level = (geologicalIndex(pos) + depth) % 20183
    pos2Erosion[pos] = level
//    println("ER: $pos -> $level")
    return level
}

boolean isRock(Pos pos) {
    erosionLevel(pos) % 3 == 0
}

boolean isWet(Pos pos) {
    erosionLevel(pos) % 3 == 1
}

boolean isNarrow(Pos pos) {
    erosionLevel(pos) % 3 == 2
}

int riskLevel = 0
for (int y = 0; y <= target.y; ++y) {
    for (int x = 0; x <= target.x; ++x) {
        int level = erosionLevel(new Pos(x, y))
        riskLevel += level % 3
    }
}

println(riskLevel)