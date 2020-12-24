package pl.touk.dpr.aoc2015

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/15/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val ingredients = parseIngredients(input)
        return calculate(ingredients, 100)
                .map { score(ingredients, it) }
                .max()!!
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
        if (ingredients.size == 1) {
            return setOf(listOf(toUse))
        } else {
            return (0..toUse).flatMap { use ->
                val toCheck = ingredients.drop(1)
                calculate(toCheck, toUse - use).map {
                    listOf(use) + it
                }
            }.toSet()
        }
    }

    private fun part2(input: List<String>): Any {
        val ingredients = parseIngredients(input)
        return calculate(ingredients, 100)
                .filter { calories(ingredients, it) == 500 }
                .map { score(ingredients, it) }
                .max()!!
    }

    private fun calories(ingredients: List<Ingredient>, amount: List<Int>): Int {
        return ingredients.map { it.calories }.zip(amount).fold(0) { acc, pair -> acc + pair.first * pair.second }
    }

    data class Ingredient(val capacity: Int, val durability: Int, val flavor: Int, val texture: Int, val calories: Int)

}