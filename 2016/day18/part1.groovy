def main(String input, int size) {
    List row = input.collect {it}
    List rows = [row]
    while(rows.size() < size){
        List begin = ['.'] + rows[rows.size() - 1] + ['.']
        rows << (begin.collate(3,1).findAll {it.size() == 3}
            .collect {isTrap(it[0], it[1], it[2]) ? '^' : '.'} )
        println rows[rows.size() -1]
    }
    println rows.join('\n')
    rows.flatten().findAll {it == '.'}.sum { 1 }
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

println(main('.^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^', 40))
