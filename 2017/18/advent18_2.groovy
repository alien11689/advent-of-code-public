String input = new File('input.txt').text

List lines = input.split('\n').collect{it.trim()}

class Register{
	Map reg = [:]

	def get(key){
		if(key.isNumber()){
			return key as long
		}
		if(key in reg){
			return reg[key] as long
		}else{
			reg[key] = 0
			return 0
		}
	}

	def put(x, y){
		reg[x] = (y as long)
	}
}


@groovy.transform.Immutable
class Snd {
	String x
	
	def apply(Context c){
		Register reg = c.reg
		c.next.mes.offer(reg.get(x))
		c.next.waiting = false
		c.send++
		return 1
	}
}

@groovy.transform.Immutable
class Set {
	String x
	String y
	def apply(Context c){
		Register reg = c.reg
		reg.put(x, reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Add {
	String x
	String y
	def apply(Context c){
		Register reg = c.reg
		reg.put(x, reg.get(x) + reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Mul {
	String x
	String y
	def apply(Context c){
		Register reg = c.reg
		reg.put(x, reg.get(x) * reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Mod {
	String x
	String y
	def apply(Context c){
		Register reg = c.reg
		reg.put(x, reg.get(x) % reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Rcv {
	String x
	def apply(Context c){
		if(c.mes.empty){
			c.waiting = true
			return 0
		}
		c.reg.put(x, c.mes.poll())
		return 1
	}
}

@groovy.transform.Immutable
class Jgz {
	String x
	String y
	def apply(Context c){
		Register reg = c.reg
		if(reg.get(x)  > 0){
			return reg.get(y)
		}
		return 1
	}
}

List actions = lines.collect {
	List parts = it.split(' ')
	if(parts[0] == 'snd'){
		new Snd(parts[1])
	} else if(parts[0] == 'set') {
		new Set(parts[1], parts[2])
	} else if(parts[0] == 'add') {
		new Add(parts[1], parts[2])
	} else if(parts[0] == 'mul') {
		new Mul(parts[1], parts[2])
	} else if(parts[0] == 'mod') {
		new Mod(parts[1], parts[2])
	} else if(parts[0] == 'jgz') {
		new Jgz(parts[1], parts[2])
	} else if(parts[0] == 'rcv') {
		new Rcv(parts[1])
	}
}

class Context {
	Register reg = new Register()
	int cur = 0
	int send = 0
	LinkedList mes = [] as LinkedList
	boolean waiting = false
	int id
	Context next

	def working(size){
		cur < size
	}
}

Context a = new Context()
a.reg.put('p', 0)
Context b = new Context(next: a)
b.reg.put('p', 1)
a.next = b

while(a.working(actions.size()) || b.working(actions.size()) ){
	while(a.cur < actions.size()){
		println "a: ${a.cur} ${actions[a.cur]} $a.mes.size"
		a.cur += actions[a.cur].apply(a)
		if(a.waiting){
			break
		}
	}
	while(b.cur < actions.size()){
		println "b: ${b.cur} ${actions[b.cur]} $b.mes.size"
		b.cur += actions[b.cur].apply(b)
		if(b.waiting){
			break
		}
	}
	if(a.waiting && b.waiting){
		break
	}
}

println "a sent: $a.send"
println "b sent: $b.send"
