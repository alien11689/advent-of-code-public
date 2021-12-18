package pl.touk.dpr.aoc2021

import java.util.Stack

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val result = lines.map { line -> readSnumber(line) }
            .map { snumbers ->
                fullReduce(snumbers)
            }
            .reduce { first, second ->
                val mm = mutableListOf<Snumber>()
                mm.add(Snumber.LeftParent)
                mm.addAll(first)
                mm.addAll(second)
                mm.add(Snumber.RightParent)
                val res = fullReduce(mm)
                printExpr(res)
                res
            }
//            .forEach(::println)

        return magnitude(result)
    }

    private fun magnitude(result: MutableList<Snumber>): Long {
        val s = Stack<Long>()
        result.forEach { snumber ->
            when (snumber) {
                Snumber.LeftParent -> Unit
                is Snumber.Num -> s.push(snumber.n.toLong())
                Snumber.RightParent -> {
                    val right = s.pop()
                    val left = s.pop()
                    s.push(left * 3 + right * 2)
                }
            }
        }
        return s.pop()
    }

    private fun printExpr(res: MutableList<Snumber>) {
        println(res.joinToString(",").replace(",]", "]").replace("[,", "["))
    }

    private fun fullReduce(snumbers: MutableList<Snumber>): MutableList<Snumber> {
        var before = snumbers.toList()
        while (true) {
//            print("Full reduce: ")
//            printExpr(snumbers)
            reduceExplode(snumbers)
//            print("After explosions: ")
//            printExpr(snumbers)
            val after = snumbers.toList()
            if (after == before) {
                reduceSplit(snumbers)
//                print("After split: ")
//                printExpr(snumbers)
                val afterSplit = snumbers.toList()
                if (afterSplit == after) {
                    break
                } else {
                    before = afterSplit
                }
            } else {
                before = after
            }
        }
        return snumbers
    }

    private fun part2(lines: List<String>): Any {
        return -1
    }

    sealed interface Snumber {
        object LeftParent : Snumber {
            override fun toString(): String {
                return "["
            }
        }

        object RightParent : Snumber {
            override fun toString(): String {
                return "]"
            }
        }

        data class Num(var n: Int) : Snumber {
            override fun toString(): String {
                return n.toString()
            }
        }
    }

    private fun readSnumber(line: String): MutableList<Snumber> {
        return line.mapNotNull { c ->
            when (c) {
                '[' -> Snumber.LeftParent
                ']' -> Snumber.RightParent
                ',' -> null
                else -> Snumber.Num(c.toString().toInt())
            }
        }.toMutableList()
    }

    private fun reduceExplode(snumbers: MutableList<Snumber>) {
        var i = 0
        var parenCount = 0
        while (i < snumbers.size) {
            val cur = snumbers[i]
            if (cur == Snumber.LeftParent) {
                ++parenCount
            } else if (cur == Snumber.RightParent) {
                --parenCount
            } else if (cur is Snumber.Num) { //number
                if (parenCount > 4) {
                    val rawNext = snumbers[i + 1]
                    if (rawNext !is Snumber.Num) {
                        ++i
                        continue
                    }
                    val next = snumbers[i + 1] as Snumber.Num
//                    print("Input: ")
//                    printExpr(snumbers)
//                    println("Explode on parentLevel $parenCount: [$cur,$next]")
                    val firstNumLeft = snumbers.take(i - 1).filter { it is Snumber.Num }.lastOrNull() as Snumber.Num?
                    if (firstNumLeft != null) {
                        firstNumLeft.n += cur.n
                    }
                    val firstNumRight = snumbers.drop(i + 3).filter { it is Snumber.Num }.firstOrNull() as Snumber.Num?
                    if (firstNumRight != null) {
                        firstNumRight.n += next.n
                    }
                    snumbers.removeAt(i + 2)
                    snumbers.removeAt(i + 1)
                    snumbers.removeAt(i)
                    snumbers.removeAt(i - 1)
                    snumbers.add(i - 1, Snumber.Num(0))
                    return
                }
            }
            ++i
        }
    }

    private fun reduceSplit(snumbers: MutableList<Snumber>) {
        var i = 0
        while (i < snumbers.size) {
            val cur = snumbers[i]
            if (cur is Snumber.Num) { //number
                if (cur.n >= 10) {
//                    print("Input: ")
//                    printExpr(snumbers)
//                    println("Split $cur")
                    snumbers.removeAt(i)
                    snumbers.addAll(i, listOf(Snumber.LeftParent, Snumber.Num(cur.n / 2), Snumber.Num((cur.n + 1) / 2), Snumber.RightParent))
                    return
                }
            }
            ++i
        }
    }
}


