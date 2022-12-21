package pl.touk.dpr.aoc2018

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/21/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val ip = input[0].split(" ").last().toInt()
        val instructions = input.drop(1).map { line ->
            val parts = line.split(" ")
            Operation(parts[0], parts[1].toInt(), parts[2].toInt(), parts[3].toInt())
        }

        val threshold = 10000
        var curMinR4 = 10000000000L
        var operations = 0
        val registers = mutableListOf(0L, 0L, 0L, 0L, 0L, 0L, 0L)
        val instrToCount = mutableMapOf<Int, Int>()
        while (true) {
            ++operations
            val cur = registers[6].toInt()
//        println("$cur: ${instructions[cur]}")
            registers[ip] = registers[6]
            instrToCount[cur] = (instrToCount[cur] ?: 0) + 1
            instructions[cur].apply(registers)
//        println(registers)
            if (cur == 28 && curMinR4 > registers[4]) {
                curMinR4 = registers[4]
//                println("In iteration ${instrToCount[28]} min is $curMinR4")
                return curMinR4
            }
            if (registers[ip] + 1 >= instructions.size) {
                throw RuntimeException("Finished")
            }
            registers[6] = registers[ip] + 1
            val count = instrToCount[28] ?: 0 > threshold
            if (count) {
                println("Break  is in loop")
                throw RuntimeException("In a loop ")
            }
        }
    }

    private fun part2(input: List<String>): Any {
        val ip = input[0].split(" ").last().toInt()
        val instructions = input.drop(1).map { line ->
            val parts = line.split(" ")
            Operation(parts[0], parts[1].toInt(), parts[2].toInt(), parts[3].toInt())
        }
        val threshold = 100000
        var operations = 0L
        val result2Iteration = mutableMapOf<Long, Long>()
        val registers = mutableListOf(0L, 0L, 0L, 0L, 0L, 0L, 0L)
        while (true) {
            val cur = registers[6]
            registers[ip] = registers[6]
//        println("$cur: ${instructions[cur.toInt()]}")
//        print("Before $registers || ")
            if (cur == 20L && registers[5] == 0L && registers[2] < registers[3]) {
                while ((registers[5] + 1) * 256 < registers[3]) {
                    operations += 7
                    ++registers[5]
                    registers[2] = (registers[5] + 1) * 256
                }
            }
            instructions[cur.toInt()].apply(registers)
//        println(registers)
            if (cur == 28L) {
                ++operations
                val current = result2Iteration[registers[4]]
                if (current == null) {
                    result2Iteration[registers[4]] = operations
//                    println("Max: ${result2Iteration.maxByOrNull {it.value}!!.key}")
                }
                if (result2Iteration.size > threshold || operations > threshold.toLong() * 100000) {
//                println("Max: ${result2Iteration.maxByOrNull {it.value}!!.key}")
                    return result2Iteration.maxByOrNull { it.value }!!.key
                }
            }
            if (registers[ip] + 1 >= instructions.size) {
//            println("Halts")
                throw RuntimeException("Halts")
            }
            registers[6] = registers[ip] + 1
        }
    }

    data class Operation(val name: String, val a: Int, val b: Int, val c: Int) {
        fun apply(registers: MutableList<Long>) {
            when (name) {
                "addr" -> registers[c] = registers[a] + registers[b]
                "addi" -> registers[c] = registers[a] + b
                "setr" -> registers[c] = registers[a]
                "seti" -> registers[c] = a.toLong()
                "mulr" -> registers[c] = registers[a] * registers[b]

                "muli" -> registers[c] = registers[a] * b
                "banr" -> registers[c] = registers[a].and(registers[b])

                "bani" -> registers[c] = registers[a] and b.toLong()
                "borr" -> registers[c] = registers[a] or registers[b]
                "bori" -> registers[c] = registers[a] or b.toLong()

                "gtir" -> registers[c] = if (a > registers[b]) 1 else 0

                "gtri" -> registers[c] = if (registers[a] > b) 1 else 0

                "gtrr" -> registers[c] = if (registers[a] > registers[b]) 1 else 0

                "eqir" -> registers[c] = if (a.toLong() == registers[b]) 1 else 0

                "eqri" -> registers[c] = if (registers[a] == b.toLong()) 1 else 0

                "eqrr" -> registers[c] = if (registers[a] == registers[b]) 1 else 0

            }
        }
    }
}
