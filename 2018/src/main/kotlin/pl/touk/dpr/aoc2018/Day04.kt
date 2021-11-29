package pl.touk.dpr.aoc2018

import java.time.Duration
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/04/input.txt").sorted()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var lastGuardId: Int? = null
        var fall: LocalDateTime? = null
        val m = mutableMapOf<Int, MutableList<Sleep>>()
        input.forEach { line ->
            val parts = line.split(Regex("[ #]+"))
                    .map { it.replace(Regex("\\["), "").replace(Regex("]"), "") }
            if (parts[2] == "Guard") {
                lastGuardId = parts[3].toInt()
                m[lastGuardId!!] = m[lastGuardId!!] ?: mutableListOf()
            } else if (parts[2] == "falls") {
                val ts = LocalDateTime.of(LocalDate.parse(parts[0]), LocalTime.parse(parts[1]))
                fall = ts
            } else {
                val ts = LocalDateTime.of(LocalDate.parse(parts[0]), LocalTime.parse(parts[1]))
                val dur = Duration.between(fall, ts).toMinutes().toInt()
                m[lastGuardId!!]!!.add(Sleep(dur, fall!!.toLocalTime(), ts.toLocalTime(), fall!!.toLocalDate()))
            }
        }

        val max = m.maxByOrNull { it.value.sumBy { it.dur } }!!
        val minutes = mutableMapOf<Int, Int>()

        max.value.forEach { s ->
            val start = s.start.minute
            val end = s.end.minute
            for (i in (0 until 60)) {
                if (i >= start && i < end) {
                    minutes[i] = minutes.getOrDefault(i, 0) + 1
                }
            }
        }
        return max.key * minutes.maxByOrNull { it.value }!!.key
    }

    private fun part2(input: List<String>): Any {
        var lastGuardId: Int? = null
        var fall: LocalDateTime? = null
        val m = mutableMapOf<Int, MutableList<IntRange>>()
        input.forEach { line ->
            val parts = line.split(Regex("[ #]+"))
                    .map { it.replace(Regex("\\["), "").replace(Regex("]"), "") }
            if (parts[2] == "Guard") {
                lastGuardId = parts[3].toInt()
                m[lastGuardId!!] = m[lastGuardId!!] ?: mutableListOf()
            } else if (parts[2] == "falls") {
                val ts = LocalDateTime.of(LocalDate.parse(parts[0]), LocalTime.parse(parts[1]))
                fall = ts
            } else {
                val ts = LocalDateTime.of(LocalDate.parse(parts[0]), LocalTime.parse(parts[1]))
                m[lastGuardId!!]!!.add(fall!!.minute..ts!!.minute)
            }
        }

        val result: Map<Int, Pair<Int,Int>> = m.filter { it.value.isNotEmpty() }.map { entry ->
            val minMap = mutableMapOf<Int, Int>()
            entry.value.forEach { r ->
                r.forEach {
                    minMap[it] = minMap.getOrDefault(it, 0) + 1
                }
            }
            Pair(entry.key, minMap.maxByOrNull { it.value }!!.toPair())
        }.toMap()
        val entry = result.maxByOrNull { it.value.second }!!
        return entry.key * entry.value.first
    }

    data class Sleep(val dur: Int, val start: LocalTime, val end: LocalTime, val date: LocalDate)
}