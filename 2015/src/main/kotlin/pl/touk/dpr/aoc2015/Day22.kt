package pl.touk.dpr.aoc2015

import java.util.PriorityQueue

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
        val pq = PriorityQueue<State>()
        pq.offer(State())
        while (pq.isNotEmpty()) {
            val curState = pq.poll()
            val nextActions = curState.nextActions()
            nextActions.forEach {
                val nextState = curState.useAction(it)
                when {
                    nextState.boss.hitPoints <= 0 -> {
                        return nextState.usedMana
                    }
                    nextState.hp > 0 -> pq.offer(nextState)
                    else -> {
                    }
                }
            }
        }
        TODO()
    }

    private fun part2(): Any {
        val pq = PriorityQueue<State>()
        pq.offer(State().hurt())
        while (pq.isNotEmpty()) {
            val curState = pq.poll()
            val nextActions = curState.nextActions()
            nextActions.forEach {
                val nextState = curState.useAction2(it)
                when {
                    nextState.boss.hitPoints <= 0 -> {
                        return nextState.usedMana
                    }
                    nextState.hp > 0 -> pq.offer(nextState)
                    else -> {
                    }
                }
            }
        }
        TODO()

    }

    data class Boss(val hitPoints: Int = 55, val damage: Int = 8)

    data class State(val rund: Int = 1, val hp: Int = 50, val mana: Int = 500, val currentEffects: Map<Action, Int> = mapOf(), val boss: Boss = Boss(), val usedMana: Int = 0, val armor: Int = 0, val usedActions: List<Action> = listOf()) : Comparable<State> {
        override fun compareTo(other: State): Int {
            if (usedMana != other.usedMana) {
                return usedMana - other.usedMana
            }
            if (boss.hitPoints != other.boss.hitPoints) {
                return boss.hitPoints - other.boss.hitPoints
            }
            if (hp != other.hp) {
                return other.hp - hp
            }
            if (mana != other.mana) {
                return mana - other.mana
            }
            return rund - other.rund
        }

        fun nextActions(): Collection<Action> = Action.values()
                .filter { it.cost < mana }
                .filter { it !in currentEffects }

        fun useAction(action: Action): State {
            return applyAction(action).applyEffects().bossTurn().applyEffects()
        }

        fun useAction2(action: Action): State {
            return applyAction(action).applyEffects().bossTurn().hurt().applyEffects()
        }

        fun hurt(): State {
            if (hp > 0 && boss.hitPoints > 0) {
                return copy(hp = hp - 1)
            }
            return this
        }

        private fun applyAction(action: Action) = when (action) {
            Action.MagicMissile -> copy(mana = mana - action.cost, usedMana = usedMana + action.cost, usedActions = usedActions + action, boss = boss.copy(hitPoints = boss.hitPoints - 4))
            Action.Drain -> copy(mana = mana - action.cost, usedMana = usedMana + action.cost, usedActions = usedActions + action, boss = boss.copy(hitPoints = boss.hitPoints - 2), hp = hp + 2)
            Action.Shield -> copy(mana = mana - action.cost, usedMana = usedMana + action.cost, usedActions = usedActions + action, currentEffects = currentEffects + Pair(action, action.time))
            Action.Poison -> copy(mana = mana - action.cost, usedMana = usedMana + action.cost, usedActions = usedActions + action, currentEffects = currentEffects + Pair(action, action.time))
            Action.Recharge -> copy(mana = mana - action.cost, usedMana = usedMana + action.cost, usedActions = usedActions + action, currentEffects = currentEffects + Pair(action, action.time))
        }

        private fun bossTurn(): State {
            if (hp > 0 && boss.hitPoints > 0) {
                return copy(hp = hp - listOf(1, boss.damage - armor).maxOrNull()!!)
            }
            return this
        }

        private fun applyEffects(): State {
            var cur = this
            cur.currentEffects.forEach { entry ->
                if (cur.hp > 0 && cur.boss.hitPoints > 0) {
                    cur = cur.applyEffect(entry.key)
                } else {
                    cur = this
                }
            }
            cur = cur.copy(currentEffects = cur.currentEffects.mapValues { it.value - 1 })
            if (cur.currentEffects[Action.Shield] ?: 0 == 0) {
                cur = cur.copy(armor = 0)
            }
            return cur.copy(currentEffects = cur.currentEffects.filter { it.value > 0 })
        }

        private fun applyEffect(action: Action): State {
            return when (action) {
                Action.Shield -> copy(armor = 7)
                Action.Poison -> copy(boss = boss.copy(hitPoints = boss.hitPoints - 3))
                Action.Recharge -> copy(mana = mana + 101)
                else -> TODO()
            }
        }
    }

    enum class Action(val cost: Int, val time: Int) {
        MagicMissile(53, 0),
        Drain(73, 0),
        Shield(113, 6),
        Poison(173, 6),
        Recharge(229, 5)
    }
}
