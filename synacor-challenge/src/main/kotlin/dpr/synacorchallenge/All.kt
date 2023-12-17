package dpr.synacorchallenge

object All {
    @JvmStatic
    fun main(args: Array<String>) {
        println("Coins")
        Coins.main(args)
        println("Teleporter")
        Teleporter.main(args)
        println("Orb")
        Orb.main(args)
        println("Whole")
        Main.main(args)
    }
}
