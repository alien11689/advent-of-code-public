package dpr.aoc2023

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
}
