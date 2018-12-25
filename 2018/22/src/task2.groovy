import groovy.transform.Canonical

depth = 510l
target = new Pos(10, 10)

depth = 11541
target = new Pos(14, 778)

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
    def value = erosionLevel(pos.left()) * erosionLevel(pos.up())
    geoIndex[pos] = value
//    println("GI: $pos -> $value")
    return value
}

def erosionLevel(Pos pos) {
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

@Canonical
class Pos implements Comparable<Pos> {
    int x
    int y

    Pos left() {
        return new Pos(x - 1, y)
    }

    Pos up() {
        return new Pos(x, y - 1)
    }

    List<Pos> neighbours() {
        [
                new Pos(x, y + 1),
                new Pos(x, y - 1),
                new Pos(x + 1, y),
                new Pos(x - 1, y),
        ].findAll {
            it.x >= 0 && it.y >= 0
        }
    }

    @Override
    int compareTo(Pos o) {
        if (y == o.y) {
            return x <=> o.x
        }
        return y <=> o.y
    }

    int dist(Pos o) {
        return Math.abs(x - o.x) + Math.abs(y - o.y)
    }

}

Type getType(Pos pos) {
    int level = erosionLevel(pos)
    if (level % 3 == 0) {
        return Type.R
    }
    if (level % 3 == 1) {
        return Type.W
    }
    if (level % 3 == 2) {
        return Type.N
    }
}

Set mem = [] as Set

enum Equip {
    TORCH,
    CLIMB_GEAR,
    NOTHING
}

@Canonical
class Here implements Comparable<Here> {
    Pos pos
    int minutes
    int distToTarget
    Equip equip
    List<Pos> path

    @Override
    int compareTo(Here o) {
        if (minutes != o.minutes) {
            return minutes <=> o.minutes
        }
        if (distToTarget != o.distToTarget) {
            return distToTarget <=> o.distToTarget
        }
        return 0//pos <=> o.pos
    }
}

PriorityQueue<Here> pq = new PriorityQueue<Here>()
Pos start = new Pos(0, 0)
pq.offer(new Here(start, 0, start.dist(target), Equip.TORCH, [start]))

enum Type {
    R,
    W,
    N
}

Map transaition = [
        [Type.R, Equip.CLIMB_GEAR, Type.R]: [Equip.CLIMB_GEAR, 1],
        [Type.R, Equip.TORCH, Type.R]     : [Equip.TORCH, 1],

        [Type.R, Equip.CLIMB_GEAR, Type.W]: [Equip.CLIMB_GEAR, 1],
        [Type.R, Equip.TORCH, Type.W]     : [Equip.CLIMB_GEAR, 8],

        [Type.R, Equip.CLIMB_GEAR, Type.N]: [Equip.TORCH, 8],
        [Type.R, Equip.TORCH, Type.N]     : [Equip.TORCH, 1],

        [Type.W, Equip.CLIMB_GEAR, Type.W]: [Equip.CLIMB_GEAR, 1],
        [Type.W, Equip.NOTHING, Type.W]   : [Equip.NOTHING, 1],

        [Type.W, Equip.CLIMB_GEAR, Type.R]: [Equip.CLIMB_GEAR, 1],
        [Type.W, Equip.NOTHING, Type.R]   : [Equip.CLIMB_GEAR, 8],

        [Type.W, Equip.CLIMB_GEAR, Type.N]: [Equip.NOTHING, 8],
        [Type.W, Equip.NOTHING, Type.N]   : [Equip.NOTHING, 1],

        [Type.N, Equip.TORCH, Type.N]     : [Equip.TORCH, 1],
        [Type.N, Equip.NOTHING, Type.N]   : [Equip.NOTHING, 1],

        [Type.N, Equip.TORCH, Type.W]     : [Equip.NOTHING, 8],
        [Type.N, Equip.NOTHING, Type.W]   : [Equip.NOTHING, 1],

        [Type.N, Equip.TORCH, Type.R]     : [Equip.TORCH, 1],
        [Type.N, Equip.NOTHING, Type.R]   : [Equip.TORCH, 8],

]

List<Pos> copyPath(List<Pos> prev, Pos cur) {
    def collect = prev.collect()
    collect << cur
    return collect
}

Here winner
int minDist = 10000
while (!pq.empty) {
//    println("Size: ${pq.size()}")
    Here cur = pq.poll()
    if(cur.distToTarget > minDist + 50){
        continue
    }
    if ([cur.pos, cur.equip] in mem) {
        continue
    }
    if(minDist > cur.distToTarget){
	    minDist = cur.distToTarget
	    }
    assert (getType(cur.pos) == Type.N && cur.equip in [Equip.TORCH, Equip.NOTHING]) || (getType(cur.pos) == Type.R && cur.equip in [Equip.TORCH, Equip.CLIMB_GEAR]) || (getType(cur.pos) == Type.W && cur.equip in [Equip.CLIMB_GEAR, Equip.NOTHING])
    if (cur.pos == target) {
        if (cur.equip == Equip.TORCH) {
//            println("Found: $cur")
            winner = cur
            break
        } else {
            pq.offer(new Here(cur.pos, cur.minutes + 7, 0, Equip.TORCH, copyPath(cur.path, cur.pos)))
            continue
        }
    }
//    println("Dist: ${cur.distToTarget}; minutes: ${cur.minutes}, cur: $cur.pos")
    mem << [cur.pos, cur.equip]
    Type curType = getType(cur.pos)
    cur.pos
            .neighbours()
//            .findAll { !(it in mem) }
            .collect { Pos next ->
        Type nextType = getType(next)
//        println("Neighbour: $next")
        def (Equip e, int dm) = transaition[[curType, cur.equip, nextType]]
//        println("$cur.pos [$curType, $cur.equip, $nextType] -> $next [$nextType, $e, $dm]")
        new Here(next, cur.minutes + dm, next.dist(target), e, copyPath(cur.path, next))
    }.each {
        pq.offer(it)
    }
}

//for (int y = 0; y <= winner.path.max {it.y}.y; ++y) {
//    for (int x = 0; x <= winner.path.max {it.x}.x; ++x) {
//        Pos pos = new Pos(x, y)
//        if (target == pos) {
//            print('T')
//        } else if (start == pos) {
//            print('M')
//        } else if (pos in winner.path) {
//            print('X')
//        } else {
//            def type = getType(pos)
//            if (type == Type.N) {
//                print('|')
//            } else if (type == Type.W) {
//                print('=')
//            } else {
//                print('.')
//            }
//        }
//    }
//    println()
//}

println(winner.minutes)
