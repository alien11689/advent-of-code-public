package pl.touk.dpr.eulerproject

object Problem0052 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        var i = 1L
        while (true){
            val x = listOf(i, i*2, i*3, i*4, i*5, i * 6)
                    .map { it.toString().toList().sorted() }
                    .toSet()
            if(x.size == 1){
                println(i)
                break
            }
            ++i
        }
    }
}
