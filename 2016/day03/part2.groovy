def main(String input) {
    println(new File(input).text
        .split('\n')
        .collect {            it.trim().split(/\s+/)        }
        .collate(3)
        .collectMany {
            def a = it[0]
            def b = it[1]
            def c = it[2]
            return [
                new Triangle(a[0] as int ,b[0] as int,c[0] as int),
                new Triangle(a[1] as int ,b[1] as int,c[1] as int),
                new Triangle(a[2] as int ,b[2] as int,c[2] as int)
            ]
        }
        .findAll {it?.valid()}
        .size())
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
