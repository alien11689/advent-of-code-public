package pl.touk.dpr.synacorchallenge

object Coins {
    @JvmStatic
    fun main(args: Array<String>) {
        val red = 2
        val blue = 9
        val corroded = 3
        val concave = 7
        val shiny = 5

        val all = setOf(red, blue, corroded, concave, shiny)
        all.forEach { c1 ->
            all.forEach { c2 ->
                all.forEach { c3 ->
                    all.forEach { c4 ->
                        all.forEach { c5 ->
                            if (setOf(c1, c2, c3, c4, c5) == all && c1 + c2 * c3 * c3 + c4 * c4 * c4 - c5 == 399) {
                                println("$c1 $c2 $c3 $c4 $c5")
                                return
                            }
                        }
                    }
                }
            }
        }
    }
}
