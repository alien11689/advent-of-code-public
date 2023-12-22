package dpr.aoc2023

import dpr.commons.Util

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/22/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/22/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Point3D(val x: Int, val y: Int, val z: Int) {
        fun down(): Point3D = copy(z = z - 1)
    }

    data class Brick(val id: Int, val blocks: List<Point3D>) {
        fun down(): Brick = copy(blocks = blocks.map { it.down() })
    }

    private fun part1(lines: List<String>): Any {
        var bricks = readBricks(lines)
        val ids = bricks.map { it.id }
        val stableBlocks = mutableMapOf<Point3D, Int>()
        while (bricks.isNotEmpty()) {
            println("UNSTABLE")
            while (true) {
                println(" Internal")
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
        val ll = stableBlocks.toList()
        val blocksNotToRemove = mutableSetOf<Int>()
        ids.forEach { id ->
            val blocksBelow = ll.filter { it.second == id }
                .mapNotNull {
                    val oneDown = it.first.down()
                    when (val below = stableBlocks[oneDown]) {
                        null, id -> null
                        else -> below
                    }
                }.toSet()
            if (blocksBelow.size == 1) {
                blocksNotToRemove.addAll(blocksBelow)
            }
        }
        return ids.size - blocksNotToRemove.size
        // 563 is not right
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

    private fun part2(lines: List<String>): Any {
        return "TODO"
    }
}

