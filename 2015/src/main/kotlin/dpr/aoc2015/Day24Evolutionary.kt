package dpr.aoc2015

object Day24Evolutionary {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val numbers = input.map { it.toInt() }.sortedBy { -it }
        val perBucket = numbers.sum() / 3
        return findBestQuantuum(numbers, perBucket)
    }

    private fun findBestQuantuum(numbers: List<Int>, perBucket: Int): Long {
        var bestSize = numbers.size
        var bestQuantum = Long.MAX_VALUE
        val mem = mutableSetOf<List<Set<Int>>>()
        repeat(5000) {
            val buckets = createNextBuckets(numbers, perBucket, mem)
            val firstSize = buckets[0].size
            val firstQuantumEntaglement = buckets[0].fold(1L) { acc, i -> acc * i }
            if (firstSize < bestSize) {
                bestSize = firstSize
                bestQuantum = firstQuantumEntaglement
//                println("$firstSize $firstQuantumEntaglement")
            } else if (firstSize == bestSize && firstQuantumEntaglement < bestQuantum) {
                bestQuantum = firstQuantumEntaglement
//                println("$firstSize $firstQuantumEntaglement")
            }
        }
        return bestQuantum
    }

    private fun createNextBuckets(numbers: List<Int>, perBucket: Int, mem: MutableSet<List<Set<Int>>>): List<Set<Int>> {
        val nums = numbers.toMutableList()
        while (true) {
            nums.shuffle()
            val first = pack(nums, perBucket)
            val second = pack(nums.filter { it !in first }, perBucket)
            val third = pack(nums.filter { it !in (first + second) }, perBucket)
            val buckets = listOf(first, second, third).sortedBy { it.size }
            if (isValid(buckets, perBucket) && buckets !in mem) {
                mem.add(buckets)
                return buckets
            }
        }
    }

    private fun isValid(buckets: List<Set<Int>>, perBucket: Int): Boolean {
        val first = buckets[0].sum()
        return first == perBucket && first == buckets[1].sum() && first == buckets[1].sum()
    }

    private fun pack(nums: List<Int>, perBucket: Int): Set<Int> {
        val ml = mutableListOf<Int>()
        nums.forEach {
            if (ml.sum() + it <= perBucket) {
                ml.add(it)
            }
        }
        return ml.toSet()
    }

    private fun part2(input: List<String>): Any {
        val numbers = input.map { it.toInt() }.sortedBy { -it }
        val perBucket = numbers.sum() / 4
        return findBestQuantuum(numbers, perBucket)
    }
}
