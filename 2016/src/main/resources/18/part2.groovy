def main(String input, int size) {
    List prev = input.collect {it}
    int count = 1
    int safe = prev.findAll {it == '.'}.sum {1}
    while(count < size){
        println count
        List begin = ['.'] + prev + ['.']
        prev = (begin.collate(3,1).findAll {it.size() == 3}
            .collect {isTrap(it[0], it[1], it[2]) ? '^' : '.'} )
        safe += prev.findAll {it == '.'}.sum {1}
        ++count
    }
    println safe
}

boolean isTrap(String l, String c, String r){
    return l == '^' && c == '^' && r == '.' ||
        l == '.' && c == '^' && r == '^' ||
        l == '.' && c == '.' && r == '^' ||
        l == '^' && c == '.' && r == '.'
}

println(main('..^^.', 3))
println()
println(main('.^^.^.^^^^', 10))
println()

println(main('.^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^', 400000))
