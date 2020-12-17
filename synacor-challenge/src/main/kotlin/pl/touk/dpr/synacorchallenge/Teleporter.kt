package pl.touk.dpr.synacorchallenge

import java.lang.RuntimeException

object Teleporter {
    @JvmStatic
    fun main(args: Array<String>) {
        val r0 = 4
        val r1 = 1
        val x = (1..32767).find {
            println("Checking $it")
            val first = solve(r0, r1, it, mutableMapOf()).first
            first == 6
        }
        println(x)
    }

    private val BASE = 32768

    private fun solve(R0: Int, R1: Int, r7: Int, mem: MutableMap<Pair<Int, Int>, Pair<Int, Int>>): Pair<Int, Int> {
//        println("solve $R0 $R1 ${mem.size}")
        val key = Pair(R0, R1)
        if (key in mem) {
            return mem[key]!!
        }
        var r0 = R0
        var r1 = R1

        if (r0 > 0) {
            val res = fun6035(r0, r1, r7, mem)
            mem[key] = res
            return res
        }
        r0 = (r1 + 1) % BASE
        mem[key] = Pair(r0, r1)
        return Pair(r0, r1)
    }

    private fun fun6035(R0: Int, R1: Int, r7: Int, mem: MutableMap<Pair<Int, Int>, Pair<Int, Int>>): Pair<Int, Int> {
//        println("fun6035 $R0 $R1")
        var r0 = R0
        var r1 = R1
        if (r1 > 0) {
            return fun6048(r0, r1, r7, mem)
        }
        r0 = (r0 + 32767) % BASE
        r1 = r7
        return solve(r0, r1, r7, mem)
    }

    private fun fun6048(R0: Int, R1: Int, r7: Int, mem: MutableMap<Pair<Int, Int>, Pair<Int, Int>>): Pair<Int, Int> {
//        println("fun6048 $R0 $R1")
        val x = R0
        var r1 = (R1 + 32767) % BASE
        var res = solve(R0, r1, r7, mem)
        var r0 = res.first
        r1 = res.second
        r1 = r0
        r0 = x
        r0 = (r0 + 32767) % BASE
        res = solve(r0, r1, r7, mem)
        return res
    }
}