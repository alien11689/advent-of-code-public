String input = new File('day12.data').text

import groovy.json.*

def json = new JsonSlurper().parseText(input)

long s = 0

Stack stack = new Stack()
stack.add(json)
while(!stack.empty){
    def cur = stack.pop()
    String className = cur.getClass().name
    println className
    if(className == 'groovy.json.internal.LazyMap' || className == 'org.apache.groovy.json.internal.LazyMap'){
        if(cur.values().find{ it == 'red'} == null) {
            stack.addAll(cur.values())
        }
    }
    if(className == 'java.util.ArrayList'){
        stack.addAll(cur)
    }
    if(className == 'java.lang.Integer'){
        s += cur as Long
    }
}

println s
