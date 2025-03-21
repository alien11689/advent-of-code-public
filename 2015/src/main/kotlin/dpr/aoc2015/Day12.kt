package dpr.aoc2015

import dpr.commons.Util

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/12/input.txt")
        val (json, _) = parseJson(input.first())
        println(part1(json))
        println(part2(json))
    }

    private fun part1(json: Json): Int {
        return sumNumbers(json)
    }

    private fun sumNumbers(json: Json): Int {
        return when (json) {
            is Json.JsonNum -> json.v
            is Json.JsonArray -> json.elems.sumOf { sumNumbers(it) }
            is Json.JsonObject -> json.elems.values.sumOf { sumNumbers(it) }
            else -> 0
        }
    }

    private fun parseJson(input: String, pos: Int = 0): Pair<Json, Int> {
        return when (input[pos]) {
            '{' -> parseObject(input, pos + 1)
            '[' -> parseArray(input, pos + 1)
            '"' -> parseString(input, pos + 1)
            else -> parseNumber(input, pos)
        }
    }

    private fun parseObject(input: String, pos: Int): Pair<Json, Int> {
        var i = pos
        val mm = mutableMapOf<String, Json>()
        while (true) {
            if (input[i] == '}') {
                return Pair(Json.JsonObject(mm.toMap()), i + 1)
            } else if (input[i] == ',') {
                val (key, nextPos) = parseString(input, i + 2)
                i = nextPos + 1
                val (value, afterValuePos) = parseJson(input, i)
                i = afterValuePos
                mm[key.v] = value
            } else {
                val (key, nextPos) = parseString(input, i + 1)
                i = nextPos + 1
                val (value, afterValuePos) = parseJson(input, i)
                i = afterValuePos
                mm[key.v] = value
            }
        }
    }

    private fun parseArray(input: String, pos: Int): Pair<Json, Int> {
        var i = pos
        val ml = mutableListOf<Json>()
        while (true) {
            i = if (input[i] == ']') {
                return Pair(Json.JsonArray(ml.toList()), i + 1)
            } else if (input[i] == ',') {
                val (json, nextPos) = parseJson(input, i + 1)
                ml.add(json)
                nextPos
            } else {
                val (json, nextPos) = parseJson(input, i)
                ml.add(json)
                nextPos
            }
        }
    }

    private fun parseString(input: String, pos: Int): Pair<Json.JsonString, Int> {
        var i = pos
        val sb = StringBuilder()
        while (i < input.length) {
            val cur = input[i]
            if (cur == '"') {
                return Pair(Json.JsonString(sb.toString()), i + 1)
            } else {
                sb.append(cur)
                ++i
            }
        }
        throw RuntimeException("Invalid end $i")
    }

    private fun parseNumber(input: String, pos: Int): Pair<Json, Int> {
        var i = pos
        var num = 0
        var negative = false
        while (i < input.length) {
            val cur = input[i]
            if (cur == '-') {
                negative = true
                ++i
            } else if (cur.isDigit()) {
                num = num * 10 + cur.toString().toInt()
                ++i
            } else {
                return Pair(Json.JsonNum(if (negative) -1 * num else num), i)
            }
        }
        throw RuntimeException("Invalid end $i")
    }

    private fun part2(json: Json): Any {
        return sumNumbersFilteringRed(json)
    }

    private fun sumNumbersFilteringRed(json: Json): Int {
        return when (json) {
            is Json.JsonNum -> json.v
            is Json.JsonArray -> json.elems.sumOf { sumNumbersFilteringRed(it) }
            is Json.JsonObject -> if (json.elems.values.contains(Json.JsonString("red"))) {
                0
            } else {
                json.elems.values.sumOf { sumNumbersFilteringRed(it) }
            }

            else -> 0
        }
    }

    sealed class Json {
        data class JsonArray(val elems: List<Json>) : Json()
        data class JsonObject(val elems: Map<String, Json>) : Json()
        data class JsonNum(val v: Int) : Json()
        data class JsonString(val v: String) : Json()
    }
}
