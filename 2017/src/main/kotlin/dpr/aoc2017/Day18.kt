package dpr.aoc2017

import dpr.commons.Util
import java.util.LinkedList

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): String {
        val actions = parseInput(input)

        val context = Context(0)
        context.next = context
        while (context.cur < actions.size) {
            val (counterIncr, res) = actions[context.cur].apply(context)
            if (res != null) {
                return res
            }
            context.cur += counterIncr
        }
        throw RuntimeException()
    }

    fun parseInput(input: List<String>): List<Action> {
        return input.map {
            val parts = it.split(" ")
            if (parts[0] == "snd") {
                Action.Snd(parts[1])
            } else if (parts[0] == "set") {
                Action.Set(parts[1], parts[2])
            } else if (parts[0] == "add") {
                Action.Add(parts[1], parts[2])
            } else if (parts[0] == "sub") {
                Action.Sub(parts[1], parts[2])
            } else if (parts[0] == "mul") {
                Action.Mul(parts[1], parts[2])
            } else if (parts[0] == "mod") {
                Action.Mod(parts[1], parts[2])
            } else if (parts[0] == "jgz") {
                Action.Jgz(parts[1], parts[2])
            } else if (parts[0] == "jnz") {
                Action.Jnz(parts[1], parts[2])
            } else if (parts[0] == "rcv") {
                Action.Rcv(parts[1])
            } else {
                throw RuntimeException(it)
            }
        }
    }

    @JvmStatic
    fun part2(input: List<String>): Int {
        val actions = parseInput(input)

        val context0 = Context(0)
        val context1 = Context(1)
        context0.reg.put("p", 0)
        context1.reg.put("p", 1)
        context0.next = context1
        context1.next = context0

        while (context0.working(actions.size) || context1.working(actions.size)) {
            while (context0.cur < actions.size) {
                val (counterIncr, _) = actions[context0.cur].apply(context0)
                if (context0.waiting) {
                    break
                }
                context0.cur += counterIncr
            }
            while (context1.cur < actions.size) {
                val (counterIncr, _) = actions[context1.cur].apply(context1)
                if (context1.waiting) {
                    break
                }
                context1.cur += counterIncr
            }
            if (context0.waiting && context1.waiting) {
                break
            }
        }
        return context1.send
    }

    class Register {
        private val reg = mutableMapOf<String, Long>()

        fun get(key: String): Long {
            if (key.matches(Regex("[-0-9]+"))) {
                return key.toLong()
            }
            return if (key in reg) {
                reg[key]!!
            } else {
                reg[key] = 0
                0
            }
        }

        fun put(x: String, y: Long) {
            reg[x] = y
        }
    }

    sealed class Action {
        abstract fun apply(c: Context): Pair<Int, String?>

        data class Snd(val x: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                return if (c.next == c) {
                    c.reg.put("lastPlayed", c.reg.get(x))
                    Pair(1, null)
                } else {
                    c.next!!.mes.offer(c.reg.get(x))
                    c.next!!.waiting = false
                    c.send++
                    Pair(1, null)
                }
            }
        }

        data class Set(val x: String, val y: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                c.reg.put(x, c.reg.get(y))
                return Pair(1, null)
            }
        }

        data class Add(val x: String, val y: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                c.reg.put(x, c.reg.get(x) + c.reg.get(y))
                return Pair(1, null)
            }
        }

        data class Sub(val x: String, val y: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                c.reg.put(x, c.reg.get(x) - c.reg.get(y))
                return Pair(1, null)
            }
        }

        data class Mul(val x: String, val y: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                c.reg.put(x, c.reg.get(x) * c.reg.get(y))
                return Pair(1, null)
            }
        }

        data class Mod(val x: String, val y: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                c.reg.put(x, c.reg.get(x) % c.reg.get(y))
                return Pair(1, null)
            }
        }

        data class Rcv(val x: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                if (c.next == c) {
                    if (c.reg.get(x) != 0L) {
                        return Pair(1, c.reg.get("lastPlayed").toString())
                    }
                    return Pair(1, null)
                } else {
                    if (c.mes.isEmpty()) {
                        c.waiting = true
                        return Pair(0, null)
                    }
                    c.reg.put(x, c.mes.poll())
                    return Pair(1, null)
                }
            }
        }

        data class Jgz(val x: String, val y: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                if (c.reg.get(x) > 0) {
                    return Pair(c.reg.get(y).toInt(), null)
                }
                return Pair(1, null)
            }
        }

        data class Jnz(val x: String, val y: String) : Action() {
            override fun apply(c: Context): Pair<Int, String?> {
                if (c.reg.get(x) != 0L) {
                    return Pair(c.reg.get(y).toInt(), null)
                }
                return Pair(1, null)
            }
        }
    }

    class Context(val id: Int) {
        val reg = Register()
        var cur = 0
        var send = 0
        val mes = LinkedList<Long>()
        var waiting = false
        var next: Context? = null

        fun working(size: Int) = cur < size
    }
}
