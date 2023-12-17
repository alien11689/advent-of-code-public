package dpr.aoc2015

import dpr.commons.Util

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/07/input.txt")
        val a = part1(input)
        println(a)
        println(part2(input, a))
    }

    private fun part1(input: List<String>): Int {
        val inputs = mapOf<String, Int>()
        val operations = readOperations(input)
        return process(operations, inputs)
    }

    private fun process(initialOperations: Set<Operation>, initialInputs: Map<String, Int>): Int {
        var operations = initialOperations
        var inputs = initialInputs
        while (operations.isNotEmpty()) {
            operations.forEach {
                val inp = it.result(inputs)
                if (inp != inputs) {
                    operations = operations - it
                }
                inputs = inp
            }
            if (inputs.containsKey("a")) {
                return inputs["a"]!!
            }
        }
        return -1
    }

    private fun readOperations(input: List<String>, overrides: Map<String, Int> = mapOf()): Set<Operation> {
        val operations = mutableSetOf<Operation>()
        input.forEach { line ->
            val parts = line.split(" ")
            if (line.contains("AND")) {
                operations.add(Operation.And(parts[0], parts[2], parts[4]))
            } else if (line.contains("OR")) {
                operations.add(Operation.Or(parts[0], parts[2], parts[4]))
            } else if (line.contains("LSHIFT")) {
                operations.add(Operation.LShift(parts[0], parts[2], parts[4]))
            } else if (line.contains("RSHIFT")) {
                operations.add(Operation.RShift(parts[0], parts[2], parts[4]))
            } else if (line.contains("NOT")) {
                operations.add(Operation.Not(parts[1], parts[3]))
            } else {
                if (parts[2] in overrides) {
                    operations.add(Operation.Inp(overrides[parts[2]].toString(), parts[2]))
                } else {
                    operations.add(Operation.Inp(parts[0], parts[2]))
                }
            }
        }
        return operations.toSet()
    }

    private fun part2(input: List<String>, a: Int): Any {
        val inputs = mapOf<String, Int>()
        val operations = readOperations(input, mapOf(Pair("b", a)))
        return process(operations, inputs)
    }

    sealed class Operation {

        abstract fun result(wires: Map<String, Int>): Map<String, Int>

        protected fun numberOrWire(name: String, wires: Map<String, Int>) =
            if (name.matches(Regex("[0-9]+"))) name.toInt() else wires[name]

        data class And(val x: String, val y: String, val target: String) : Operation() {
            override fun result(wires: Map<String, Int>): Map<String, Int> {
                val a = numberOrWire(x, wires)
                val b = numberOrWire(y, wires)
                if (a != null && b != null) {
                    return wires + Pair(target, a.and(b))
                }
                return wires
            }

        }

        data class Or(val x: String, val y: String, val target: String) : Operation() {
            override fun result(wires: Map<String, Int>): Map<String, Int> {
                val a = numberOrWire(x, wires)
                val b = numberOrWire(y, wires)
                if (a != null && b != null) {
                    return wires + Pair(target, a.or(b))
                }
                return wires
            }
        }

        data class LShift(val x: String, val y: String, val target: String) : Operation() {
            override fun result(wires: Map<String, Int>): Map<String, Int> {
                val a = numberOrWire(x, wires)
                val b = numberOrWire(y, wires)
                if (a != null && b != null) {
                    return wires + Pair(target, a.shl(b))
                }
                return wires
            }
        }

        data class RShift(val x: String, val y: String, val target: String) : Operation() {
            override fun result(wires: Map<String, Int>): Map<String, Int> {
                val a = numberOrWire(x, wires)
                val b = numberOrWire(y, wires)
                if (a != null && b != null) {
                    return wires + Pair(target, a.shr(b))
                }
                return wires
            }
        }

        data class Not(val x: String, val target: String) : Operation() {
            override fun result(wires: Map<String, Int>): Map<String, Int> {
                val a = numberOrWire(x, wires)
                if (a != null) {
                    return wires + Pair(target, 65535 - a)
                }
                return wires
            }
        }

        data class Inp(val x: String, val target: String) : Operation() {
            override fun result(wires: Map<String, Int>): Map<String, Int> {
                val a = numberOrWire(x, wires)
                if (a != null) {
                    return wires + Pair(target, a)
                }
                return wires
            }
        }

    }
}
