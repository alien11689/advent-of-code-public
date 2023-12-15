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
        return lines.sumOf { line ->
            line.split(",").sumOf { hash(it) }
        }
    }

    private fun hash(s: String): Int {
        return s.fold(0) { acc, c -> (acc + c.code) * 17 % 256 }
    }

    data class Lens(val name: String, var value: Int)

    data class Box(val lenses: MutableList<Lens> = mutableListOf())

    private fun part2(lines: List<String>): Any {
        val boxes = mutableListOf<MutableList<Lens>>()
        repeat(256) { boxes.add(mutableListOf()) }
        lines.forEach { line ->
            line.split(",").forEach { operation ->
                when {
                    operation.endsWith("-") -> {
                        val name = operation.substring(0, operation.length - 1)
                        boxes[hash(name)].removeIf { it.name == name }
                    }

                    operation.contains("=") -> {
                        val (name, value) = operation.split("=")
                        val lenses = boxes[hash(name)]
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
        return boxes.flatMapIndexed { boxId, lenses -> lenses.mapIndexed { lId, lens -> 1L * (boxId + 1) * (lId + 1) * lens.value } }.sum()
    }
}

