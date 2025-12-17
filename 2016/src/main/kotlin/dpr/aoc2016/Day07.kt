package dpr.aoc2016

import dpr.commons.Util

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/07/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Int {
        return input.asSequence()
            .map { Ip.from(it) }
            .count { it.supportsTls() }
    }

    private fun part2(input: List<String>): Int {
        return input.asSequence()
            .map { Ip.from(it) }
            .count { it.supportsSsl() }
    }

    data class Ip(val abba: List<String>, val bridges: List<String>) {
        fun supportsTls(): Boolean {
            return abba.any { hasPalindrom(it) } && bridges.none { hasPalindrom(it) }
        }

        private fun hasPalindrom(s: String): Boolean {
            var i = 0
            while (i < s.length - 3) {
                if (s[i] == s[i + 3] && s[i + 1] == s[i + 2] && s[i] != s[i + 1]) {
                    return true
                }
                ++i
            }
            return false
        }

        fun supportsSsl(): Boolean {
            val aba = abba.flatMap { getAbaReversed(it) }
            return bridges.any { b -> aba.any { b.contains(it) } }
        }

        private fun getAbaReversed(s: String): List<String> {
            return (0..(s.length - 3)).flatMap { idx ->
                val sub = s.substring(idx, idx + 3)
                if (sub[0] == sub[2] && sub[0] != sub[1]) {
                    listOf(listOf(sub[1], sub[0], sub[1]).joinToString(""))
                } else {
                    listOf()
                }
            }
        }

        companion object {
            fun from(input: String): Ip {
                val parts = input.split(Regex("[\\[\\]]+"))
                return Ip(parts.filterIndexed { index, _ -> index % 2 == 0 }, parts.filterIndexed { index, _ -> index % 2 == 1 })
            }
        }
    }
}
