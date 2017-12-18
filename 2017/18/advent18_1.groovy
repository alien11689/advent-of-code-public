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

Register reg = new Register()

@groovy.transform.Immutable
class Snd {
	String x
	
	def apply(Register reg){
		reg.put('lastPlayed', reg.get(x))
		return 1
	}
}

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
class Add {
	String x
	String y
	def apply(Register reg){
		reg.put(x, reg.get(x) + reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Mul {
	String x
	String y
	def apply(Register reg){
		reg.put(x, reg.get(x) * reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Mod {
	String x
	String y
	def apply(Register reg){
		reg.put(x, reg.get(x) % reg.get(y))
		return 1
	}
}

@groovy.transform.Immutable
class Rcv {
	String x
	def apply(Register reg){
		if(reg.get(x) != 0){
			throw new Exception(reg.get('lastPlayed') as String)
		}
		return 1
	}
}

@groovy.transform.Immutable
class Jgz {
	String x
	String y
	def apply(Register reg){
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

int cur = 0
while(cur < actions.size()){
	println actions[cur]
	cur += actions[cur].apply(reg)
}
