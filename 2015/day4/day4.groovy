String key = 'iwrupvqb'

import java.security.MessageDigest

def generateMD5_A(String s){
    MessageDigest.getInstance("MD5").digest(s.bytes).encodeHex().toString()
}
(1..10000000).each{
	String md5 = generateMD5_A(key + it)
	if(md5[0..4] == '00000'){
		println it
	}
}

