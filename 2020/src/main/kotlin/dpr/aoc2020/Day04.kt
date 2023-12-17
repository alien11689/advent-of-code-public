package dpr.aoc2020

import dpr.commons.Util

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getLinesFromFile("/04/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
        var valid = 0
        var current = mutableSetOf<String>()
        input.forEach { line ->
            if (line.isEmpty()) {
                valid += if (current.size == 8 || current.size == 7 && !current.contains("cid")) 1 else 0
                current = mutableSetOf()
            } else {
                line.splitToSequence(" ")
                    .map { it.split(":")[0] }
                    .forEach { current.add(it) }
            }
        }
        println(valid)
    }

    private fun part2(input: List<String>) {
        var valid = 0
        var current = mutableMapOf<String, String>()
        input.forEach { line ->
            if (line.isEmpty()) {
                val id = Id.fromMap(current)
                valid += if (id.valid()) 1 else 0
                current = mutableMapOf()
            } else {
                line.splitToSequence(" ")
                    .forEach { field ->
                        val split = field.split(":")
                        current[split[0]] = split[1]
                    }
            }
        }
        println(valid)
    }

    data class Id(
        val byr: String?, val iyr: String?, val eyr: String?,
        val hgt: String?, val hcl: String?, val ecl: String?,
        val pid: String?, val cid: String?
    ) {
        fun valid(): Boolean {
            if (byr == null) {
                return false
            }
            val byrInt = byr.toInt()
            if (byrInt < 1920 || byrInt > 2002) {
                return false
            }

            if (iyr == null) {
                return false
            }
            val iyrInt = iyr.toInt()
            if (iyrInt < 2010 || iyrInt > 2020) {
                return false
            }

            if (eyr == null) {
                return false
            }
            val eyrInt = eyr.toInt()
            if (eyrInt < 2020 || eyrInt > 2030) {
                return false
            }

            if (hgt == null) {
                return false
            }
            if (hgt.endsWith("cm")) {
                val height = hgt.subSequence(0, hgt.length - 2).toString().toInt()
                if (height < 150 || height > 193) {
                    return false
                }
            } else if (hgt.endsWith("in")) {
                val height = hgt.subSequence(0, hgt.length - 2).toString().toInt()
                if (height < 59 || height > 76) {
                    return false
                }
            } else {
                return false
            }

            if (ecl == null) {
                return false
            }
            if (ecl !in setOf("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) {
                return false
            }

            if (pid == null) {
                return false
            }
            if (pid.length != 9) {
                return false
            }
            if (!pid.all { it.isDigit() }) {
                return false
            }

            if (hcl == null) {
                return false
            }
            if (!hcl.matches(Regex("^#[0-9a-f]{6}$"))) {
                return false
            }

            return true
        }

        companion object {
            fun fromMap(fields: Map<String, String>): Id = Id(
                fields["byr"], fields["iyr"], fields["eyr"],
                fields["hgt"], fields["hcl"], fields["ecl"],
                fields["pid"], fields["cid"]
            )
        }
    }

}

