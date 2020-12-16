package pl.touk.dpr.synacorchallenge

object Teleporter {
    @JvmStatic
    fun main(args: Array<String>) {
        val r0 = 4
        val r1 = 1
        (1..32767).find {
            println("Checking $it")
            solve(r0, r1, it, mutableMapOf()).first == 6
        }
    }

    private val BASE = 32768

    private fun solve(startR0: Int, startR1: Int, r7: Int, mem: MutableMap<Pair<Int, Int>, Pair<Int, Int>>): Pair<Int, Int> {
        var r0 = startR0
        var r1 = startR1

        if (r0 != 0) {
            val key = Pair(r0, r1)
            val res = if (mem.contains(key)) mem[key]!! else fun6035(r0, r1, r7, mem)
            mem[key] = res
            r0 = res.first
            r1 = res.second
        }
        r0 = (r1 + 1) % BASE
        return Pair(r0, r1)
    }

    private fun fun6035(R0: Int, R1: Int, r7: Int, mem: MutableMap<Pair<Int, Int>, Pair<Int, Int>>): Pair<Int, Int> {
        var r0 = R0
        var r1 = R1
        if (r0 > 0) {
            val res = fun6048(r0, r1, r7, mem)
            r0 = res.first
            r1 = res.second
        }
        r0 = (r0 + 32767) % BASE
        r1 = r7
        return solve(r0, r1, r7, mem)
    }

    private fun fun6048(R0: Int, R1: Int, r7: Int, mem: MutableMap<Pair<Int, Int>, Pair<Int, Int>>): Pair<Int, Int> {
        val x = R0
        var r1 = (R0 + 32767) % BASE
        var res = solve(R0, r1, r7, mem)
        var r0 = res.first
        r1 = res.second
        r1 = r0
        r0 = x
        r0 = (r0 + 32767) % BASE
        res = solve(R0, r1, r7, mem)
        return res
    }
}