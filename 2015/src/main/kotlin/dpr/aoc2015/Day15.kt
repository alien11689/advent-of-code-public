package dpr.aoc2015

import dpr.commons.Util

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/15/input.txt")
        val ingredients = parseIngredients(input)
        println(part1(ingredients))
        println(part2(ingredients))
    }

    private fun part1(ingredients: List<Ingredient>): Any {
        return calculate(ingredients, 100)
            .maxOf { score(ingredients, it) }
    }

    private fun parseIngredients(input: List<String>): List<Ingredient> {
        val ingredients = input.map {
            val parts = it.split(Regex("[ ,:]+"))
            Ingredient(parts[2].toInt(), parts[4].toInt(), parts[6].toInt(), parts[8].toInt(), parts[10].toInt())
        }
        return ingredients
    }

    private fun score(ingredients: List<Ingredient>, amount: List<Int>): Int {
        val capacity = ingredients.map { it.capacity }.zip(amount).fold(0) { acc, pair -> acc + pair.first * pair.second }
        val durability = ingredients.map { it.durability }.zip(amount).fold(0) { acc, pair -> acc + pair.first * pair.second }
        val flavor = ingredients.map { it.flavor }.zip(amount).fold(0) { acc, pair -> acc + pair.first * pair.second }
        val texture = ingredients.map { it.texture }.zip(amount).fold(0) { acc, pair -> acc + pair.first * pair.second }
        return setOf(capacity, durability, flavor, texture).map { if (it > 0) it else 0 }.fold(1) { acc, i -> acc * i }
    }

    private fun calculate(ingredients: List<Ingredient>, toUse: Int): Set<List<Int>> {
        return if (ingredients.size == 1) {
            setOf(listOf(toUse))
        } else {
            (0..toUse).flatMap { use ->
                val toCheck = ingredients.drop(1)
                calculate(toCheck, toUse - use).map {
                    listOf(use) + it
                }
            }.toSet()
        }
    }

    private fun part2(ingredients: List<Ingredient>): Any {
        return calculate(ingredients, 100)
            .filter { calories(ingredients, it) == 500 }
            .maxOf { score(ingredients, it) }
    }

    private fun calories(ingredients: List<Ingredient>, amount: List<Int>): Int {
        return ingredients.map { it.calories }.zip(amount).fold(0) { acc, pair -> acc + pair.first * pair.second }
    }

    data class Ingredient(val capacity: Int, val durability: Int, val flavor: Int, val texture: Int, val calories: Int)

}
