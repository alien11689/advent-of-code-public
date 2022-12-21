package pl.touk.dpr.aoc2017

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/21/input.txt")
        part1And2(input).forEach { println(it) }
    }

    private fun part1And2(lines: List<String>): Collection<Any> {
        var image = listOf(".#.".toList(), "..#".toList(), "###".toList())
        val result = mutableListOf<Int>()

        val rules = lines.map { line ->
            var (ins, out) = line.split(" => ")
            val input = ins.split('/').map { it.toList() }
            val output = out.split('/').map { it.toMutableList() }.toMutableList()
            val inputs = listOf(
                    input,
                    input.map { it.reversed() },
                    transpose(transpose(input).map { it.reversed() }),
            ).flatMap {
                listOf(it, rotate(it), rotate(rotate(it)), rotate(rotate(rotate(it))))
            }.toSet()
            Rule(
                    output.size - 1,
                    inputs,
                    output
            )
        }

        var iter = 0
        while (iter < 18) {
            ++iter
            val newImage = mutableListOf<MutableList<Char>>()
            val split = if (image.size % 2 == 0) 2 else 3
            var i = 0
            while (i < image.size) {
                val parts = mutableListOf<MutableList<MutableList<Char>>>()
                var j = 0
                while (j < image.size) {
                    val part: List<List<Char>> = (i until (i + split)).map {
                        image[it].subList(j, j + split)
                    }
                    val rule = rules.find { it.match(part) }!!
                    val result = rule.output
                    parts.add(result)
                    j += split
                }
                newImage.addAll(parts.fold(if(parts[0].size == 3) mutableListOf(mutableListOf<Char>(),mutableListOf<Char>(),mutableListOf<Char>()) else mutableListOf(mutableListOf<Char>(),mutableListOf<Char>(),mutableListOf<Char>(), mutableListOf<Char>())) { acc, cur ->
                    (0 until cur.size).forEach {
                        acc[it].addAll(cur[it])
                    }
                    acc
                })
                i += split
            }
//            newImage.forEach { row ->
//                row.forEach { print(it) }
//                println()
//            }
            image = newImage
            if (iter in setOf(5, 18)) {
                result.add(image.flatten().filter { it == '#' }.size)
            }
            if (result.size == 2) {
                break
            }
        }
        return result
    }

    data class Rule(val size: Int, val inputs: Set<List<List<Char>>>, val output: MutableList<MutableList<Char>>) {
        fun match(image: List<List<Char>>): Boolean {
            return image.size == size && image in inputs
        }
    }

    private fun rotate(grid: List<List<Char>>): List<List<Char>> {
        val matrix = grid.map { it.toMutableList() }.toMutableList()
        val length = matrix.size - 1;

        var i = 0
        while (i <= (length) / 2) {
            var j = i
            while (j < length - i) {

                //Coordinate 1
                val p1 = matrix[i][j]

                //Coordinate 2
                val p2 = matrix[j][length - i];

                //Coordinate 3
                val p3 = matrix[length - i][length - j];

                //Coordinate 4
                val p4 = matrix[length - j][i];

                //Swap values of 4 coordinates.
                matrix[j][length - i] = p1;
                matrix[length - i][length - j] = p2;
                matrix[length - j][i] = p3;
                matrix[i][j] = p4;

                ++j
            }
            ++i
        }
        return matrix
    }

    private fun transpose(input: List<List<Char>>): List<List<Char>> {
        val out = (0 until input[0].size).map {
            (0 until input.size).map {
                'a'
            }.toMutableList()
        }.toMutableList()
        for (i in input.indices) {
            for (j in input[i].indices) {
                out[j][i] = input[i][j]
            }
        }
        return out
    }
}
