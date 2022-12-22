package pl.touk.dpr.aoc2018

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/03/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(lines: List<String>): Any {
        val size = 1000
        val carpets = createCarpets(lines)
        val field = createField(size, carpets)

        return field.flatten().filter { it.ids.size > 1 }.size
    }

    private fun createField(size: Int, carpets: List<Carpet>): List<List<Cell>> {
        val field = (1..size).map { (1..size).map { Cell() } }
        carpets.forEach {
            (it.fromTop until it.fromTop + it.height).forEach { i ->
                (it.fromLeft until it.fromLeft + it.width).forEach { j ->
                    field[i][j].ids.add(it.id)
                }
            }
        }
        return field
    }

    private fun createCarpets(lines: List<String>) = lines.map {
        val parts = it.split(Regex("[ #@,:x]"))
        Carpet(id = parts[1], fromLeft = parts[4].toInt(),
                fromTop = parts[5].toInt(), width = parts[7].toInt(),
                height = parts[8].toInt())
    }

    private fun part2(lines: List<String>): Any {
        val size = 1000
        val carpets = createCarpets(lines)
        val field = createField(size, carpets)

        val flattenOneField = field.flatten().filter { it.ids.size == 1 }

        val idsToCheck = flattenOneField.flatMap { it.ids }.toSet()

        carpets.filter { it.id in idsToCheck }.map { c ->
            if (flattenOneField.filter { it.contains(c.id) }.size == c.height * c.width) {
                return c.id
            }
        }
        throw RuntimeException()
    }

    data class Carpet(val id: String, val fromLeft: Int, val fromTop: Int, val width: Int, val height: Int)

    data class Cell(val ids: MutableSet<String> = mutableSetOf()) {
        fun contains(id: String): Boolean {
            return id in ids
        }
    }
}
