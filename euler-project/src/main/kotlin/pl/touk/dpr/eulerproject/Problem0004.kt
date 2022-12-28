package pl.touk.dpr.eulerproject

object Problem0004 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        var x = 999
        var maxPalindrom = 0
        while (x >= 100) {
            var y = 999
            while (y >= 100) {
                val xy = x * y
                if (xy <= maxPalindrom) {
                    break
                }
                if (xy.toString() == xy.toString().reversed()) {
                    maxPalindrom = xy
                    break
                }
                --y
            }
            --x
        }
        println(maxPalindrom)
    }
}
