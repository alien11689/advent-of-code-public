package dpr.aoc2016

import java.math.BigInteger
import java.security.MessageDigest
import java.util.LinkedList

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "mmsxrhfx"
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val pos = Pos(path = input)
        val q = LinkedList<Pos>()
        q.add(pos)
        while (true) {
            val cur = q.poll()
            if (cur.x == 3 && cur.y == 3) {
                return cur.path.substring(input.length)
            }
            processMoves(cur, q)
        }
    }

    private fun part2(input: String): Any {
        var pos = Pos(path = input)
        val q = LinkedList<Pos>()
        q.add(pos)
        while (q.isNotEmpty()) {
            val cur = q.poll()
            if (cur.x == 3 && cur.y == 3) {
                pos = cur
                continue
            }
            processMoves(cur, q)
        }
        return pos.path.substring(input.length).length
    }

    private fun processMoves(cur: Pos, q: LinkedList<Pos>) {
        val available = md5(cur.path).slice((0..3)).map {
            it in setOf('b', 'c', 'd', 'e', 'f')
        }
        val moves = listOfNotNull(
                if (available[0]) 'U' else null,
                if (available[1]) 'D' else null,
                if (available[2]) 'L' else null,
                if (available[3]) 'R' else null,
        )

        moves.map { newPos(cur, it) }.filter { it.isValid() }.forEach {
            q.offer(it)
        }
    }

    private fun newPos(pos: Pos, x: Char): Pos {
        return Pos(
                x = pos.x + (if (x == 'L') -1 else if (x == 'R') 1 else 0),
                y = pos.y + if (x == 'U') -1 else if (x == 'D') 1 else 0,
                path = pos.path + x,
                size = pos.size + 1
        )
    }

    data class Pos(val x: Int = 0, val y: Int = 0, val path: String = "", val size: Int = 0) {
        fun isValid() = x in (0..3) && y in (0..3)
    }

    private fun md5(s: String): String {
        md.reset()
        return String.format("%032x", BigInteger(1, md.digest(s.toByteArray())))
    }

    private val md = MessageDigest.getInstance("MD5")
}
