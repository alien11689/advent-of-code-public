package dpr.aoc2023

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
            fun fromChar(c: Char): CardType = when (c) {
                '2' -> _2
                '3' -> _3
                '4' -> _4
                '5' -> _5
                '6' -> _6
                '7' -> _7
                '8' -> _8
                '9' -> _9
                'T' -> T
                'J' -> J
                'Q' -> Q
                'K' -> K
                else -> A
            }

            fun fromCharWithJoker(c: Char): CardType = when (c) {
                '2' -> _2
                '3' -> _3
                '4' -> _4
                '5' -> _5
                '6' -> _6
                '7' -> _7
                '8' -> _8
                '9' -> _9
                'T' -> T
                'J' -> JOKER
                'Q' -> Q
                'K' -> K
                else -> A
            }

        }
    }

    data class Card(val elements: Map<CardType, Int>, val text: String, val withJoker: Boolean) : Comparable<Card> {

        val type: Int = calculateType(elements)
        private val cardTypes = text.map { if (withJoker) CardType.fromCharWithJoker(it) else CardType.fromChar(it) }
        val realType: Int = if (withJoker) upgradeType() else type

        private fun upgradeType(): Int {
            if (type == 7) {
                return 7
            }
            if (elements.none { it.key == CardType.JOKER }) {
                return type
            }
            return text.toCharArray().filter { it != 'J' }.toSet().maxOf {
                from(text.replace('J', it), withJoker).type
            }
        }

        private fun calculateType(elements: Map<CardType, Int>): Int {
            val values = elements.values
            return when {
                5 in values -> 7
                4 in values -> 6
                3 in values && 2 in values -> 5
                3 in values -> 4
                2 in values && values.size == 3 -> 3
                2 in values && values.size == 4 -> 2
                else -> 1
            }
        }

        companion object {
            fun from(s: String, withJoker: Boolean): Card {
                val elements = s.toCharArray().groupBy { it }
                    .map { (if (withJoker) CardType.fromCharWithJoker(it.key) else CardType.fromChar(it.key)) to it.value.size }.toMap()
                return Card(elements, s, withJoker)
            }
        }

        override fun compareTo(other: Card): Int {
            if (realType != other.realType) {
                return realType.compareTo(other.realType)
            } else {
                val level1 = cardTypes[0].compareTo(other.cardTypes[0])
                return if (level1 == 0) {
                    val level2 = cardTypes[1].compareTo(other.cardTypes[1])
                    return if (level2 == 0) {
                        val level3 = cardTypes[2].compareTo(other.cardTypes[2])
                        return if (level3 == 0) {
                            val level4 = cardTypes[3].compareTo(other.cardTypes[3])
                            return if (level4 == 0) cardTypes[4].compareTo(other.cardTypes[4]) else level4
                        } else level3
                    } else level2
                } else level1
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

