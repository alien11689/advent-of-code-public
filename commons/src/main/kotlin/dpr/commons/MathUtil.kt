package dpr.commons

import kotlin.math.sqrt

object MathUtil {
    fun lowestCommonMultiple(x: Long, y: Long): Long {
        return (x * y) / greatestCommonDivisor(x, y);
    }

    fun greatestCommonDivisor(xx: Long, yy: Long): Long {
        var x = xx
        var y = yy
        while (x != y) {
            if (x > y)
                x -= y;
            else
                y -= x;
        }
        return x;
    }

    fun isPrime(n: Long): Boolean {
        when {
            n < 2L -> return false
            n == 2L || n == 3L -> return true
            else -> {
                var i = 2L
                while (i <= sqrt(n.toDouble())) {
                    if (n % i == 0L) {
                        return false
                    }
                    ++i
                }
            }
        }
        return true
    }

    fun properDividers(value: Int): Set<Int> {
        val divs = mutableSetOf(1)
        var i = 2
        while (i <= sqrt(value.toDouble())) {
            if (value % i == 0) {
                divs.add(i)
                divs.add(value / i)
            }
            ++i
        }
        return divs
    }
}
