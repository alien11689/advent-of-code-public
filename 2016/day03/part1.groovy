def main(String input) {
    println new File(input).text
        .split('\n')
        .collect {
            if(!it) {return null}
            def (a,b,c) = it.trim().split(/\s+/)
            return new Triangle(a as int ,b as int,c as int)
        }
        .findAll {it?.valid()}
        .size()
}

@groovy.transform.Immutable
class Triangle {
    int a,b,c

    def valid(){
        a + b > c && a + c > b && b + c > a
    }
}

main('sample.txt')
println()
main('input.txt')
