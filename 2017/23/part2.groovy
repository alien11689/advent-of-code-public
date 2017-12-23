String input = new File('inputFixed.txt').text

List lines = input.split('\n').collect{it.trim()}

class Register{
	Map reg = [
		a:1,
		b: 0,
		c: 0,
		d: 0,
		e: 0,
		f: 0,
		g: 0,
		h: 0,
	]


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

Register reg = new Register()

@groovy.transform.Immutable
class Set {
	String x
	String y
	def apply(Register reg){
		reg.put(x, reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Sub {
	String x
	String y
	def apply(Register reg){
		reg.put(x, reg.get(x) - reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Mul {
	static int count = 0
	String x
	String y
	def apply(Register reg){
		count++
		reg.put(x, reg.get(x) * reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Jnz {
	String x
	String y
	def apply(Register reg){
		if(reg.get(x)  != 0){
			return reg.get(y)
		}
		return 1
	}
}

List actions = lines.collect {
	List parts = it.split(' ')
	if(parts[0] == 'set') {
		new Set(parts[1], parts[2])
	} else if(parts[0] == 'sub') {
		new Sub(parts[1], parts[2])
	} else if(parts[0] == 'mul') {
		new Mul(parts[1], parts[2])
	} else if(parts[0] == 'jnz') {
		new Jnz(parts[1], parts[2])
	}
}

@groovy.transform.Immutable
class Entry {
	int cur
	Map reg
}

int cur = 0
while(cur < actions.size()){
	def r = reg.reg
	println "${cur+1}: ${actions[cur]} ${r}"
//	Thread.sleep(100)
	if(cur == 19 && r.get('g') != 0) {
		r.put('e', r.get('e') - r.get('g') - 1)
		r.put('g', -1)
		//reg.reg.put('d', reg.reg.get('b') - 1)
		//reg.reg.put('f', 0)
	}
	cur += actions[cur].apply(reg)
}

println reg.get('h')
