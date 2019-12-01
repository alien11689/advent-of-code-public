def lines = new File('input.txt').text.split().collect {it as int}

int calculateFuel(int value) {
    return ((int) value / 3) - 2
}

println lines.inject(0) { a,v ->
        a + calculateFuel(v)
}

