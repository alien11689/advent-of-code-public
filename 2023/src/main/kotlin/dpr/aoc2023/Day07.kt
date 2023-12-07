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

    enum class CardType2 {
        J,
        _2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        T,
        Q,
        K,
        A;

        companion object {
            fun fromChar(c: Char): CardType2 = when (c) {
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

    data class Card2(val elements: Map<CardType2, Int>, val text: String) : Comparable<Card2> {

        val type: Int = calculateType(elements)
        val pairs = elements.toList().sortedBy { -it.first.ordinal }.sortedBy { -it.second }
        val cardTypes = text.map { CardType2.fromChar(it) }

        val upgradedType: Int = upgradeType()

        private fun upgradeType(): Int {
            if (type == 7) {
                return 7
            }
            if (elements.none { it.key == CardType2.J }) {
                return type
            }
            return text.toCharArray().filter { it != 'J' }.toSet().maxOf {
                val newCard = Card.from(text.replace('J', it))
//                println("$text ($type) could be ${newCard.text} (${newCard.type})")
                newCard.type
            }
//            val best = pairs.first { it.first != CardType2.J }.first
//            val countJ = elements[CardType2.J]!!
//            val bestElements = elements.toMutableMap()
//            bestElements.remove(CardType2.J)
//            bestElements[best] = bestElements[best]!! + countJ
//            val newType = calculateType(bestElements)
//            println("$elements ($type) upgraded to ${bestElements} ($newType)")
//            return newType
        }

        private fun calculateType(elements: Map<CardType2, Int>): Int {
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
            fun from(s: String): Card2 {
                return Card2(s.toCharArray().groupBy { it }.map { CardType2.fromChar(it.key) to it.value.size }.toMap(), s)
            }
        }

        override fun compareTo(other: Card2): Int {
            if (upgradedType != other.upgradedType) {
                return upgradedType.compareTo(other.upgradedType)
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
        val cardsToPoints = lines.map { it.split(Regex("\\s+")) }.map { Card.from(it[0]) to it[1].toLong() }
        return cardsToPoints.sortedBy { it.first }.mapIndexed { i, cur -> (i + 1) * cur.second }.sum()
        // 251420309 is too high
        // 251133479 is too high
        // 251195241
    }

    private fun part2(lines: List<String>): Any {
        val cardsToPoints = lines.map { it.split(Regex("\\s+")) }.map { Card2.from(it[0]) to it[1].toLong() }
//        cardsToPoints.sortedBy { it.first }.forEach { println("${it.first.text}: " + (it.first.pairs to it.second) + " (${it.first.type} -> ${it.first.upgradedType})") }
        return cardsToPoints.sortedBy { it.first }.mapIndexed { i, cur -> (i + 1) * cur.second }.sum()
        // is too low 249723387
    }
}

