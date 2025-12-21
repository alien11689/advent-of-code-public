package dpr.aoc2017

import dpr.commons.Util

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/23/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Int {
        val actions = Day18.parseInput(input)
        val context = Day18.Context(0)
        var mul = 0
        while (context.cur < actions.size) {
            val action = actions[context.cur]
            if (action is Day18.Action.Mul) {
                ++mul
            }
            context.cur += action.apply(context).first
        }
        return mul
    }

    @JvmStatic
    fun part2(input: List<String>): Long {
        val actions = Day18.parseInput(input)
        val context = Day18.Context(0)
        context.reg.put("a", 1)
        while (context.cur < actions.size) {
            if (context.cur == 11) {
                val b = context.reg.get("b")
                val d = context.reg.get("d")
                val e = context.reg.get("e")
                val div = b / d
                if (b % d == 0L && div >= e && d * div == b) {
                    context.reg.put("f", 0)
                    context.cur = 24
                    continue
                }
                context.cur = 20
                continue
            }
            if (context.cur == 20) {
                var d = context.reg.get("d")
                val b = context.reg.get("b")
                while (d < b) {
                    ++d
                    if (b % d == 0L) {
                        context.reg.put("d", d)
                        break
                    }
                }
                ++context.cur
                continue
            }
            context.cur += actions[context.cur].apply(context).first
        }

        return context.reg.get("h")
    }
}
