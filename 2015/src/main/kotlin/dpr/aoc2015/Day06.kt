package dpr.aoc2015

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val lights = arrayListOf<MutableList<Boolean>>()
        (0..999).forEach { _ ->
            val l = arrayListOf<Boolean>()
            (0..999).forEach { _ ->
                l.add(false)
            }
            lights.add(l)
        }
        input.forEach { instr ->
            val parts = instr.split(Regex("[ ,]+"))
            if (parts[0].startsWith("turn")) {
                val x1 = parts[2].toInt()
                val y1 = parts[3].toInt()
                val x2 = parts[5].toInt()
                val y2 = parts[6].toInt()
                (x1..x2).forEach { x ->
                    (y1..y2).forEach { y ->
                        lights[x][y] = parts[1] == "on"
                    }
                }
            } else {
                val x1 = parts[1].toInt()
                val y1 = parts[2].toInt()
                val x2 = parts[4].toInt()
                val y2 = parts[5].toInt()
                (x1..x2).forEach { x ->
                    (y1..y2).forEach { y ->
                        lights[x][y] = !lights[x][y]
                    }
                }
            }
        }
        return lights.flatten().count { it }
    }

    private fun part2(input: List<String>): Any {
        val lights = arrayListOf<MutableList<Int>>()
        (0..999).forEach { _ ->
            val l = arrayListOf<Int>()
            (0..999).forEach { _ ->
                l.add(0)
            }
            lights.add(l)
        }
        input.forEach { instr ->
            val parts = instr.split(Regex("[ ,]+"))
            if (parts[0].startsWith("turn")) {
                val x1 = parts[2].toInt()
                val y1 = parts[3].toInt()
                val x2 = parts[5].toInt()
                val y2 = parts[6].toInt()
                (x1..x2).forEach { x ->
                    (y1..y2).forEach { y ->
                        if (parts[1] == "on") {
                            lights[x][y] += 1
                        } else {
                            val v = lights[x][y] + if (parts[1] == "on") 1 else -1
                            lights[x][y] = if (v < 0) 0 else v
                        }
                    }
                }
            } else {
                val x1 = parts[1].toInt()
                val y1 = parts[2].toInt()
                val x2 = parts[4].toInt()
                val y2 = parts[5].toInt()
                (x1..x2).forEach { x ->
                    (y1..y2).forEach { y ->
                        lights[x][y] += 2
                    }
                }
            }
        }
        return lights.flatten().sum()
    }
}
