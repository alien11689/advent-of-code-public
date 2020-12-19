import java.security.MessageDigest

def main(String salt) {
//	def mem = [:]
	def toSearch = []
	def found = []
	int i = 0
	while(true){
		String toHash = salt + i
		String hash = md5_2017(toHash)
//		mem[toHash] = hash
		def triple = hasTriple(hash)
		if(triple){
			toSearch << [triple[0], ((i+1)..(i + 1000)), i]
			toSearch.findAll {!(it in found) && i in it[1]}.each {s ->
					if(hasFive(hash, s[0] as char)){
						println s
						found << s	
					}	
			}
						if(found.size() >= 64){
							println (found.sort {it[2]}[63])
							break
						}
		}

		++i
	}
}

def hasTriple(String hash){
	hash.collect {it as char}.collate(3, 1).find {it.size() == 3 && it[0] == it[1] && it[1] == it[2]}
}

def hasFive(String hash, char x){
	hash.collect {it as char}.collate(5, 1).find {it.size() == 5 && it[0] == x && it[0] == it[1] && it[1] == it[2] && it[2] == it[3] && it[3]==it[4]}
}

def generateMD5(String s) {
	    MessageDigest digest = MessageDigest.getInstance("MD5")
            digest.update(s.bytes);
            new BigInteger(1, digest.digest()).toString(16).padLeft(32, '0')
}

def md5_2017(String s){
	String newS = s
	for (int i = 0; i < 2017; ++i){
		newS = generateMD5(newS)	
	}
	return newS	
}

//println(main('abc'))
println()
println(main('yjdafjpo'))
