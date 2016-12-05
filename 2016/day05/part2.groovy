import java.security.MessageDigest

def main(String input) {
    String[] pass = new String[8]
    int i = 0
    int size = 0
    while(size < 8){
        String hash = generateMD5(input+i)
        if(hash[0..<5] == '00000'){
            println "$i: $hash"
            int pos = Integer.parseInt(hash[5],16)
            if(pos < 8 && !pass[pos]){
                pass[pos] = hash[6]
                ++size
            }
        }
        ++i
    }
    return (pass as List<String>).join('')
}

def generateMD5(String s) {
    MessageDigest digest = MessageDigest.getInstance("MD5")
        digest.update(s.bytes);
            new BigInteger(1, digest.digest()).toString(16).padLeft(32, '0')
             } 

println(main('abc'))
println()
println(main('ojvtpuvg'))
