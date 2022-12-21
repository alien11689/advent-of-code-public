package pl.touk.dpr.aoc2020

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/21/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val ingredientList = input.map { IngredientsList.from(it) }
        val glossary = buildGlossary(ingredientList)
        return ingredientList.flatMap { it.ingredients }.filter { it !in glossary.values.toSet() }.count()
    }

    private fun buildGlossary(ingredientList: List<IngredientsList>): MutableMap<String, String> {
        val allergens = ingredientList.flatMap { it.allergens }.toSet()
        val allIngre = ingredientList.flatMap { it.ingredients }.toSet()
        val glossary = mutableMapOf<String, String>()
        while (glossary.size < allergens.size) {
            allergens.forEach { allerg ->
                if (allerg !in glossary) {
                    val possibleDishes = ingredientList.filter { allerg in it.allergens }.map { it.ingredients }
                    val couldBeIn = possibleDishes.fold(allIngre) { acc, set -> acc.intersect(set) } - glossary.values.toSet()
                    if (couldBeIn.size == 1) {
                        glossary[allerg] = couldBeIn.first()
                    }
                }
            }
        }
        return glossary
    }

    private fun part2(input: List<String>): Any {
        val ingredientList = input.map { IngredientsList.from(it) }
        val glossary = buildGlossary(ingredientList)
        return glossary.keys.sorted().map { glossary[it]!! }.joinToString(",")
    }

    data class IngredientsList(val ingredients: Set<String>, val allergens: Set<String> = setOf()) {
        companion object {
            fun from(inp: String): IngredientsList {
                if (inp.contains("(contains")) {
                    val ingre = mutableSetOf<String>()
                    val allergens = mutableSetOf<String>()
                    var readingAllergens = false
                    inp.split(" ").forEach { part ->
                        if (part == "(contains") {
                            readingAllergens = true
                        } else if (readingAllergens) {
                            allergens.add(part.removeSuffix(")").removeSuffix(","))
                        } else {
                            ingre.add(part)
                        }
                    }
                    return IngredientsList(ingre.toSet(), allergens.toSet())
                } else {
                    return IngredientsList(inp.split(" ").toSet())
                }
            }
        }
    }
}
