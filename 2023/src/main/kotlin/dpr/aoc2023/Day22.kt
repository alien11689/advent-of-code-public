package dpr.aoc2023

import dpr.commons.Point3D
import dpr.commons.Util

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/22/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/22/test1.txt")
        val (part1, part2) = part1And2(lines)
        println(part1)
        println(part2)
    }

    data class Brick(val id: Int, val blocks: List<Point3D>) {
        fun down(): Brick = copy(blocks = blocks.map { it.down() })
    }

    private fun findBlocksNotToRemove(stableBlocks: MutableMap<Point3D, Int>, ids: List<Int>): MutableSet<Int> {
        val ll = stableBlocks.toList().groupBy({ it.second }) { it.first }
        val blocksNotToRemove = mutableSetOf<Int>()
        ids.forEach { id ->
            val blocksBelow = ll[id]!!.mapNotNull {
                val oneDown = it.down()
                when (val below = stableBlocks[oneDown]) {
                    null, id -> null
                    else -> below
                }
            }.toSet()
            if (blocksBelow.size == 1) {
                blocksNotToRemove.addAll(blocksBelow)
            }
        }
        return blocksNotToRemove
    }

    private fun findStableBlocks(initialBricks: List<Brick>): MutableMap<Point3D, Int> {
        var bricks = initialBricks
        val stableBlocks = mutableMapOf<Point3D, Int>()
        while (bricks.isNotEmpty()) {
//            println("UNSTABLE")
            while (true) {
//                println(" Internal")
                val unstableBricks = mutableListOf<Brick>()
                bricks.forEach { brick ->
                    if (brick.blocks.any { it.z == 1 || it.down() in stableBlocks }) {
                        brick.blocks.forEach {
                            stableBlocks[it] = brick.id
                        }
                    } else {
                        unstableBricks.add(brick)
                    }
                }
                if (bricks == unstableBricks) {
                    break
                }
                bricks = unstableBricks
            }
            bricks = bricks.map { it.down() }
        }
        return stableBlocks
    }

    private fun readBricks(lines: List<String>) = lines.mapIndexed { y, line ->
        val parts = line.split(Regex("[,~]"))
        val blocks = (parts[0].toInt()..parts[3].toInt()).flatMap { x ->
            (parts[1].toInt()..parts[4].toInt()).flatMap { y ->
                (parts[2].toInt()..parts[5].toInt()).map { z ->
                    Point3D(x, y, z)
                }
            }
        }
        Brick(y, blocks)
    }

    private fun part1And2(lines: List<String>): Pair<Any, Any> {
        val initialBricks = readBricks(lines)
        val ids = initialBricks.map { it.id }
        val stableBlocks = findStableBlocks(initialBricks)
        val bricksNotToRemove = findBlocksNotToRemove(stableBlocks, ids)
        val part1Result = ids.size - bricksNotToRemove.size
        val stableBlocksAsSet = stableBlocks.toList().toSet()
        val stableBricks = stableBlocksAsSet.groupBy { it.second }.map { (id, blocks) -> Brick(id, blocks.map { it.first }) }
        return part1Result to bricksNotToRemove.sumOf { brickIdToRemove ->
//            println("Checking ${++i}/${bricksNotToRemove.size}")
            val bricks = stableBricks.filter { it.id != brickIdToRemove }
            val resAfterRemove = findStableBlocks(bricks)
            val diff = resAfterRemove.toList().toSet() - stableBlocksAsSet
            diff.map { it.second }.toSet().size
        }
    }
}

