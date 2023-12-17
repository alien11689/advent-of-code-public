package dpr.aoc2023

import dpr.commons.Util

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/07/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/07/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    enum class CardType {
        JOKER,
        _2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        T,
        J,
        Q,
        K,
        A;

        companion object {
            fun fromChar(c: Char, withJoker: Boolean): CardType = when (c) {
                '2' -> _2
                '3' -> _3
                '4' -> _4
                '5' -> _5
                '6' -> _6
                '7' -> _7
                '8' -> _8
                '9' -> _9
                'T' -> T
                'J' -> if (withJoker) JOKER else J
                'Q' -> Q
                'K' -> K
                else -> A
            }
        }
    }

    data class Card(val elements: Map<CardType, Int>, val text: String, val withJoker: Boolean) : Comparable<Card> {

        val type: Int = calculateType(elements)
        private val cardTypes = text.map { CardType.fromChar(it, withJoker) }
        private val realType: Int = if (withJoker) upgradeType() else type

        private fun upgradeType(): Int {
            if (elements.size == 1) {
                return type
            }
            if (elements.none { it.key == CardType.JOKER }) {
                return type
            }
            return text.toCharArray().filter { it != 'J' }.toSet().maxOf {
                from(text.replace('J', it), withJoker).type
            }
        }

        private fun calculateType(elements: Map<CardType, Int>): Int {
            return elements.values.sumOf { it * it }
        }

        companion object {
            fun from(s: String, withJoker: Boolean): Card {
                val elements = s.toCharArray().groupBy { it }
                    .map { (CardType.fromChar(it.key, withJoker = withJoker)) to it.value.size }.toMap()
                return Card(elements, s, withJoker)
            }
        }

        override fun compareTo(other: Card): Int {
            if (realType != other.realType) {
                return realType.compareTo(other.realType)
            } else {
                for ((l, r) in cardTypes.zip(other.cardTypes)) {
                    val res = l.compareTo(r)
                    if (res != 0) {
                        return res
                    }
                }
                return 0
            }
        }
    }


    private fun part1(lines: List<String>): Any {
        val cardsToPoints = lines.map { it.split(Regex("\\s+")) }.map { Card.from(it[0], withJoker = false) to it[1].toLong() }
        return cardsToPoints.sortedBy { it.first }.mapIndexed { i, cur -> (i + 1) * cur.second }.sum()
        // 251420309 is too high
        // 251133479 is too high
        // 251195241
    }

    private fun part2(lines: List<String>): Any {
        val cardsToPoints = lines.map { it.split(Regex("\\s+")) }.map { Card.from(it[0], withJoker = true) to it[1].toLong() }
        return cardsToPoints.sortedBy { it.first }.mapIndexed { i, cur -> (i + 1) * cur.second }.sum()
        // is too low 249723387
    }
}

