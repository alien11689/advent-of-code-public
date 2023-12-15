package dpr.aoc2023

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/15/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/15/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        // 467627 is too low
        return lines.sumOf { line ->
            line.split(",").sumOf { hash(it) }
        }
    }

    private fun hash(s: String): Long {
        return s.fold(0L) { acc, c -> (acc + c.code) * 17 % 256 }
//            .also { println("For $s -> $it") }
    }

    data class Lens(val name: String, var value: Int)

    data class Box(val lenses: MutableList<Lens> = mutableListOf())

    private fun part2(lines: List<String>): Any {
        val boxes = mutableMapOf<Int, Box>()
        repeat(256) { i -> boxes[i] = Box() }
        lines.forEach { line ->
            val operations = line.split(",")
            operations.forEach {
                when {
                    it.endsWith("-") -> {
                        val name = it.substring(0, it.length - 1)
                        val h = hash(name).toInt()
                        boxes[h]!!.lenses.removeIf { it.name == name }
                    }

                    it.contains("=") -> {
                        val (name, value) = it.split("=")
                        val h = hash(name).toInt()
                        val lenses = boxes[h]!!.lenses
                        val lens = lenses.firstOrNull { it.name == name }
                        if (lens != null) {
                            lens.value = value.toInt()
                        } else {
                            lenses.add(Lens(name, value.toInt()))
                        }
                    }
                }
            }
        }
        return (0..255).sumOf { boxId -> boxes[boxId]!!.lenses.mapIndexed { lId, lens -> 1L * (boxId + 1) * (lId + 1) * lens.value }.sum() }
    }
}

