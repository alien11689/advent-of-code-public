List<List<String>> v = new File('input.txt').text.split('\n').collect {it.split('\\)')}
//List<List<String>> v = new File('input2.txt').text.split('\n').collect {it.split('\\)')}

Map m = ['YOU': 0]

Stack stack = new Stack()
stack.push('YOU')

while(!stack.empty()){
    String cur = stack.pop()
    int count = m[cur]
    def nodes = v.findAll {it[0] == cur}
    v.removeAll(nodes)
    nodes.each{n ->
        if(!(n[1] in m)){
        stack.push(n[1])
        m[n[1]] = count + 1
        }
    }
    nodes = v.findAll {it[1] == cur}
    v.removeAll(nodes)
    nodes.each{n ->
        if(!(n[0] in m)){
        stack.push(n[0])
        m[n[0]] = count + 1
        }
    }
}
println (m['SAN'] - 2)
