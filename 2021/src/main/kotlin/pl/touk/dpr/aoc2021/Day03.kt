package pl.touk.dpr.aoc2021

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/03/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Int {
        var gamma = ""
        var epsilon = ""
        for (i in lines[0].indices) {
            val ones = lines.map { it[i] }.count { it == '1' }
            val zeros = lines.map { it[i] }.count { it == '0' }
            if (ones > zeros) {
                gamma += "1"
                epsilon += "0"
            } else {
                gamma += "0"
                epsilon += "1"
            }
        }
        return Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
    }

    private fun part2(lines: List<String>): Int {
        var oxygen = ""
        var numbers = lines
        for (i in lines[0].indices) {
            if (numbers.size == 1) {
                oxygen = numbers.first()
                break
            }
            val ones = numbers.map { it[i] }.count { it == '1' }
            val zeros = numbers.map { it[i] }.count { it == '0' }
            numbers = numbers.filter { it[i] == (if (ones >= zeros) '1' else '0') }
        }
        if(oxygen == ""){
            oxygen = numbers.first()
        }

        var co2 = ""
        numbers = lines
        for (i in lines[0].indices) {
            if (numbers.size == 1) {
                co2 = numbers.first()
                break
            }
            val ones = numbers.map { it[i] }.count { it == '1' }
            val zeros = numbers.map { it[i] }.count { it == '0' }
            numbers = numbers.filter { it[i] == (if (ones < zeros) '1' else '0') }
        }
        if(co2 == ""){
            co2 = numbers.first()
        }
        return Integer.parseInt(oxygen, 2) * Integer.parseInt(co2, 2)
    }

}

