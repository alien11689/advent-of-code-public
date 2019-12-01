def lines = new File('input.txt').text.split().collect {it as int}

def calculateFuel(int value) {
    return ((int) value / 3) - 2
}

int sum = 0
lines.each { v ->
    while(v > 0 ){
        int newV = calculateFuel(v)
        if(newV > 0){
          sum += newV
        }
        v = newV
    }
}

println sum
