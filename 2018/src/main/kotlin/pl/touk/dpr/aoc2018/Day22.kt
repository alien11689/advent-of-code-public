package pl.touk.dpr.aoc2018

import java.util.PriorityQueue
import kotlin.math.abs

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
        val pos2Erosion = mutableMapOf<Pos, Long>()
        val geoIndex = mutableMapOf<Pos, Long>()
        val target = Pos(14, 778)
        val depth = 11541
        var riskLevel = 0L
        for (y in 0..target.y) {
            for (x in 0..target.x) {
                val level = erosionLevel(Pos(x, y), pos2Erosion, geoIndex, target, depth)
                riskLevel += level % 3
            }
        }
        return riskLevel
    }

    private fun part2(): Any {
        val pos2Erosion = mutableMapOf<Pos, Long>()
        val geoIndex = mutableMapOf<Pos, Long>()
        val target = Pos(14, 778)
        val depth = 11541
        val mem = mutableSetOf<Pair<Pos, Equip>>()

        val pq = PriorityQueue<Here>()
        val start = Pos(0, 0)
        pq.offer(Here(start, 0, start.dist(target), Equip.TORCH, listOf(start)))

        val transition = mapOf(
            Transition(Type.R, Equip.CLIMB_GEAR, Type.R) to (Equip.CLIMB_GEAR to 1),
            Transition(Type.R, Equip.TORCH, Type.R) to (Equip.TORCH to 1),
            Transition(Type.R, Equip.CLIMB_GEAR, Type.W) to (Equip.CLIMB_GEAR to 1),
            Transition(Type.R, Equip.TORCH, Type.W) to (Equip.CLIMB_GEAR to 8),
            Transition(Type.R, Equip.CLIMB_GEAR, Type.N) to (Equip.TORCH to 8),
            Transition(Type.R, Equip.TORCH, Type.N) to (Equip.TORCH to 1),
            Transition(Type.W, Equip.CLIMB_GEAR, Type.W) to (Equip.CLIMB_GEAR to 1),
            Transition(Type.W, Equip.NOTHING, Type.W) to (Equip.NOTHING to 1),
            Transition(Type.W, Equip.CLIMB_GEAR, Type.R) to (Equip.CLIMB_GEAR to 1),
            Transition(Type.W, Equip.NOTHING, Type.R) to (Equip.CLIMB_GEAR to 8),
            Transition(Type.W, Equip.CLIMB_GEAR, Type.N) to (Equip.NOTHING to 8),
            Transition(Type.W, Equip.NOTHING, Type.N) to (Equip.NOTHING to 1),
            Transition(Type.N, Equip.TORCH, Type.N) to (Equip.TORCH to 1),
            Transition(Type.N, Equip.NOTHING, Type.N) to (Equip.NOTHING to 1),
            Transition(Type.N, Equip.TORCH, Type.W) to (Equip.NOTHING to 8),
            Transition(Type.N, Equip.NOTHING, Type.W) to (Equip.NOTHING to 1),
            Transition(Type.N, Equip.TORCH, Type.R) to (Equip.TORCH to 1),
            Transition(Type.N, Equip.NOTHING, Type.R) to (Equip.TORCH to 8),
        )

        var minDist = 1000
        while (!pq.isEmpty()) {
//    println("Size: ${pq.size()}")
            val cur = pq.poll()
            if (cur.distToTarget > minDist + 50) {
                continue
            }
            if (cur.pos to cur.equip in mem) {
                continue
            }
            if (minDist > cur.distToTarget) {
                minDist = cur.distToTarget
            }
            if (cur.pos == target) {
                if (cur.equip == Equip.TORCH) {
//            println("Found: $cur")
                    return cur.minutes
                } else {
                    pq.offer(Here(cur.pos, cur.minutes + 7, 0, Equip.TORCH, cur.path + cur.pos))
                    continue
                }
            }
//    println("Dist: ${cur.distToTarget}; minutes: ${cur.minutes}, cur: $cur.pos")
            mem.add(cur.pos to cur.equip)
            val curType = getType(cur.pos, geoIndex, target, pos2Erosion, depth)
            cur.pos.neighbours()
//            .findAll { !(it in mem) }
                .map { next ->
                    val nextType = getType(next, geoIndex, target, pos2Erosion, depth)
//        println("Neighbour: $next")
                    val (e, dm) = transition[Transition(curType, cur.equip, nextType)]!!
//        println("$cur.pos [$curType, $cur.equip, $nextType] -> $next [$nextType, $e, $dm]")
                    Here(next, cur.minutes + dm, next.dist(target), e, cur.path + next)
                }.forEach {
                    pq.offer(it)
                }
        }
        throw RuntimeException()
    }

    data class Transition(val from: Type, val with: Equip, val to: Type)

    data class Pos(val x: Int, val y: Int) : Comparable<Pos> {
        fun left() = Pos(x - 1, y)
        fun up() = Pos(x, y - 1)

        fun neighbours(): List<Pos> {
            return listOf(
                Pos(x, y + 1),
                Pos(x, y - 1),
                Pos(x + 1, y),
                Pos(x - 1, y),
            ).filter {
                it.x >= 0 && it.y >= 0
            }
        }

        override fun compareTo(o: Pos): Int {
            if (y == o.y) {
                return x.compareTo(o.x)
            }
            return y.compareTo(o.y)
        }

        fun dist(o: Pos) = abs(x - o.x) + abs(y - o.y)

    }

    private fun erosionLevel(
        pos: Pos,
        pos2Erosion: MutableMap<Pos, Long>,
        geoIndex: MutableMap<Pos, Long>,
        target: Pos,
        depth: Int
    ): Long {
        if (pos.x < 0 || pos.y < 0) {
            return 0
        }
        if (pos in pos2Erosion) {
            return pos2Erosion[pos]!!
        }
        val level = (geologicalIndex(pos, geoIndex, target, pos2Erosion, depth) + depth) % 20183
        pos2Erosion[pos] = level
        return level
    }

    fun geologicalIndex(
        pos: Pos,
        geoIndex: MutableMap<Pos, Long>,
        target: Pos,
        pos2Erosion: MutableMap<Pos, Long>,
        depth: Int
    ): Long {
        if (pos in geoIndex) {
            return geoIndex[pos]!!
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
            val value = pos.x * 16807L
            geoIndex[pos] = value
//        println("GI: $pos -> $value")
            return value
        }
        if (pos.x == 0) {
            val value = pos.y * 48271L
            geoIndex[pos] = value
//        println("GI: $pos -> $value")
            return value
        }
        val value = erosionLevel(pos.left(), pos2Erosion, geoIndex, target, depth) * erosionLevel(
            pos.up(),
            pos2Erosion,
            geoIndex,
            target,
            depth
        )
        geoIndex[pos] = value
//    println("GI: $pos -> $value")
        return value
    }

    enum class Equip {
        TORCH,
        CLIMB_GEAR,
        NOTHING
    }

    fun getType(
        pos: Pos, geoIndex: MutableMap<Pos, Long>,
        target: Pos,
        pos2Erosion: MutableMap<Pos, Long>,
        depth: Int
    ): Type {
        val level = erosionLevel(pos, pos2Erosion, geoIndex, target, depth)
        if (level % 3 == 0L) {
            return Type.R
        }
        if (level % 3 == 1L) {
            return Type.W
        }
        if (level % 3 == 2L) {
            return Type.N
        }
        throw RuntimeException()
    }

    enum class Type {
        R,
        W,
        N
    }

    data class Here(val pos: Pos, val minutes: Int, val distToTarget: Int, val equip: Equip, val path: List<Pos>) :
        Comparable<Here> {
        override fun compareTo(o: Here): Int {
            if (minutes != o.minutes) {
                return minutes.compareTo(o.minutes)
            }
            if (distToTarget != o.distToTarget) {
                return distToTarget.compareTo(o.distToTarget)
            }
            return 0
        }
    }
}