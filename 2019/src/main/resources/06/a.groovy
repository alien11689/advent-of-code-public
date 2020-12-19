List<List<String>> v = new File('input.txt').text.split('\n').collect {it.split('\\)')}
//List<List<String>> v = new File('input2.txt').text.split('\n').collect {it.split('\\)')}

Map m = ['COM': 0]

Stack stack = new Stack()
stack.push('COM')

println(v)

while(!stack.empty()){
    println "Stack size: ${stack.size()}"
    String cur = stack.pop()
    int count = m[cur]
    println "Cur: $cur"
    def nodes = v.findAll {it[0] == cur}
    println nodes
    v.removeAll(nodes)
    nodes.each{n ->
        stack.push(n[1])
        m[n[1]] = count + 1
    }
}
// 420029 is too high
println "Sum"
println (m.values().findAll {it > 0}.collect {it }.sum())
