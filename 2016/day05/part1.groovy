import java.security.MessageDigest

def main(String input) {
    List<String> pass = []
    int i = 0
    while(pass.size() < 8){
        String hash = generateMD5(input+i)
        //println "$i: $hash"
        if(hash[0..<5] == '00000'){
            println "$i: $hash"
            pass << hash[5]
        }
        ++i
    }
    return pass.join('')
}

def generateMD5(String s) {
    MessageDigest digest = MessageDigest.getInstance("MD5")
        digest.update(s.bytes);
            new BigInteger(1, digest.digest()).toString(16).padLeft(32, '0')
             } 

println(main('abc'))
println()
println(main('ojvtpuvg'))
