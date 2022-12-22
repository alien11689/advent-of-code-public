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
        return ingredientList.flatMap { it.ingredients }.count { it !in glossary.values.toSet() }
    }

    private fun buildGlossary(ingredientList: List<IngredientsList>): MutableMap<String, String> {
        val allergens = ingredientList.flatMap { it.allergens }.toSet()
        val allIngredients = ingredientList.flatMap { it.ingredients }.toSet()
        val glossary = mutableMapOf<String, String>()
        while (glossary.size < allergens.size) {
            allergens.forEach { allergen ->
                if (allergen !in glossary) {
                    val possibleDishes = ingredientList.filter { allergen in it.allergens }.map { it.ingredients }
                    val couldBeIn = possibleDishes.fold(allIngredients) { acc, set -> acc.intersect(set) } - glossary.values.toSet()
                    if (couldBeIn.size == 1) {
                        glossary[allergen] = couldBeIn.first()
                    }
                }
            }
        }
        return glossary
    }

    private fun part2(input: List<String>): Any {
        val ingredientList = input.map { IngredientsList.from(it) }
        val glossary = buildGlossary(ingredientList)
        return glossary.keys.sorted().joinToString(",") { glossary[it]!! }
    }

    data class IngredientsList(val ingredients: Set<String>, val allergens: Set<String> = setOf()) {
        companion object {
            fun from(inp: String): IngredientsList {
                if (inp.contains("(contains")) {
                    val ingredient = mutableSetOf<String>()
                    val allergens = mutableSetOf<String>()
                    var readingAllergens = false
                    inp.split(" ").forEach { part ->
                        if (part == "(contains") {
                            readingAllergens = true
                        } else if (readingAllergens) {
                            allergens.add(part.removeSuffix(")").removeSuffix(","))
                        } else {
                            ingredient.add(part)
                        }
                    }
                    return IngredientsList(ingredient.toSet(), allergens.toSet())
                } else {
                    return IngredientsList(inp.split(" ").toSet())
                }
            }
        }
    }
}
