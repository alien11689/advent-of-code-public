package pl.touk.dpr.aoc2015

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
        val itemSets: MutableList<Set<Item>> = generateItemSets()
        return itemSets
                .filter { fight(it) }
                .map { it.map { it.cost }.sum() }
                .min()!!
    }

    private fun generateItemSets(): MutableList<Set<Item>> {
        val itemSets: MutableList<Set<Item>> = mutableListOf<Set<Item>>()
        weapons().forEach { weapon ->
            armors().forEach { armor ->
                generateRingsSet(rings()).forEach { rings ->
                    itemSets.add(setOf(weapon, armor) + rings)
                }
            }
            generateRingsSet(rings()).forEach { rings ->
                itemSets.add(setOf(weapon) + rings)
            }
        }
        return itemSets
    }

    private fun generateRingsSet(rings: List<Item>): List<Set<Item>> {
        val ml = mutableListOf<Set<Item>>()
        ml.add(setOf())
        for (i in rings.indices) {
            ml.add(setOf(rings[i]))
            for (j in ((i + 1)..rings.size - 1)) {
                ml.add(setOf(rings[i], rings[j]))
            }
        }
        return ml
    }

    private fun fight(items: Collection<Item>): Boolean {
        val damage = items.map { it.damage }.sum()
        val armor = items.map { it.armor }.sum()
        var boss = Boss()
        var hitPoints = 100
        while (true) {
            val hit = listOf(damage - boss.armor).map { if (it >= 1) it else 1 }.first()
            boss = boss.copy(hitPoints = boss.hitPoints - hit)
            if (boss.hitPoints <= 0) {
                return true
            }
            val bossHit = listOf(boss.damage - armor).map { if (it >= 1) it else 1 }.first()
            hitPoints -= bossHit
            if (hitPoints <= 0) {
                return false
            }
        }
    }

    private fun part2(): Any {
        val itemSets: MutableList<Set<Item>> = generateItemSets()
        return itemSets
                .filter { !fight(it) }
                .map { it.map { it.cost }.sum() }
                .max()!!
    }

    private fun weapons(): List<Item> = listOf(
            Item(ItemType.WEAPON, 8, 4, 0),
            Item(ItemType.WEAPON, 10, 5, 0),
            Item(ItemType.WEAPON, 25, 6, 0),
            Item(ItemType.WEAPON, 40, 7, 0),
            Item(ItemType.WEAPON, 74, 8, 0),
    )

    private fun armors(): List<Item> = listOf(
            Item(ItemType.ARMOR, 13, 0, 1),
            Item(ItemType.ARMOR, 31, 0, 2),
            Item(ItemType.ARMOR, 53, 0, 3),
            Item(ItemType.ARMOR, 75, 0, 4),
            Item(ItemType.ARMOR, 102, 0, 5),
    )

    private fun rings(): List<Item> = listOf(
            Item(ItemType.RING, 25, 1, 0),
            Item(ItemType.RING, 50, 2, 0),
            Item(ItemType.RING, 100, 3, 0),
            Item(ItemType.RING, 20, 0, 1),
            Item(ItemType.RING, 40, 0, 2),
            Item(ItemType.RING, 80, 0, 3),
    )

    enum class ItemType {
        WEAPON,
        ARMOR,
        RING
    }

    data class Item(val type: ItemType, val cost: Int, val damage: Int, val armor: Int)

    data class Boss(val hitPoints: Int = 104, val damage: Int = 8, val armor: Int = 1)
}