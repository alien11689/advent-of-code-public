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
        }
    }

    data class Card(val elements: Map<CardType, Int>, val text: String) : Comparable<Card> {

        val type: Int = calculateType(elements)
        val pairs = elements.toList().sortedBy { -it.first.ordinal }.sortedBy { -it.second }
        val cardTypes = text.map { CardType.fromChar(it) }

        private fun calculateType(elements: Map<CardType, Int>): Int {
            val values = elements.values.sorted()
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
            fun from(s: String): Card {
                return Card(s.toCharArray().groupBy { it }.map { CardType.fromChar(it.key) to it.value.size }.toMap(), s)
            }
        }

        override fun compareTo(other: Card): Int {
            if (type != other.type) {
                return type.compareTo(other.type)
            } else {
//                val level1 = pairs[0].first.compareTo(other.pairs[0].first)
//                return if (level1 == 0) {
//                    val level2 = pairs[1].first.compareTo(other.pairs[1].first)
//                    return if (level2 == 0) {
//                        val level3 = pairs[2].first.compareTo(other.pairs[2].first)
//                        return if (level3 == 0) {
//                            val level4 = pairs[3].first.compareTo(other.pairs[3].first)
//                            return if (level4 == 0) pairs[4].first.compareTo(other.pairs[4].first) else level4
//                        } else level3
//                    } else level2
//                } else level1
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

//            return when (type) {
//                7 -> pairs[0].first.compareTo(other.pairs[0].first)
//                6, 5 -> {
//                    val highest = pairs[0].first.compareTo(other.pairs[0].first)
//                    if (highest == 0)
//                        pairs[1].first.compareTo(other.pairs[1].first)
//                    else highest
//                }
//
//                4, 3 -> {
//                    val highest = pairs[0].first.compareTo(other.pairs[0].first)
//                    return if (highest == 0) {
//                        val level2 = pairs[1].first.compareTo(other.pairs[1].first)
//                        if (level2 == 0) pairs[2].first.compareTo(other.pairs[2].first) else level2
//                    } else highest
//                }
//
//                2 -> {
//                    val highest = pairs[0].first.compareTo(other.pairs[0].first)
//                    return if (highest == 0) {
//                        val level2 = pairs[1].first.compareTo(other.pairs[1].first)
//                        return if (level2 == 0) {
//                            val level3 = pairs[2].first.compareTo(other.pairs[2].first)
//                            return if (level3 == 0) pairs[3].first.compareTo(other.pairs[3].first) else level3
//                        } else level2
//                    } else highest
//                }
//
//                1 -> {
//                    val highest = pairs[0].first.compareTo(other.pairs[0].first)
//                    return if (highest == 0) {
//                        val level2 = pairs[1].first.compareTo(other.pairs[1].first)
//                        return if (level2 == 0) {
//                            val level3 = pairs[2].first.compareTo(other.pairs[2].first)
//                            return if (level3 == 0) {
//                                val level4 = pairs[3].first.compareTo(other.pairs[3].first)
//                                return if (level4 == 0) pairs[4].first.compareTo(other.pairs[4].first) else level4
//                            } else level3
//                        } else level2
//                    } else highest
//                }
//
//                else -> 0
//            }
        }
    }

    private fun part1(lines: List<String>): Any {
        val cardsToPoints = lines.map { it.split(Regex("\\s+")) }.map { Card.from(it[0]) to it[1].toLong() }
        cardsToPoints.sortedBy { it.first }.forEach { println(it.first.pairs to it.second) }
        return cardsToPoints.sortedBy { it.first }.mapIndexed { i, cur -> (i + 1) * cur.second }.sum()
        // 251420309 is too high
        // 251133479 is too high
        // 251195241
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

