int start = 156218
int end = 652527

boolean isPassword(int [] password){
    Map m = [(password[0]):1]
    for (int i =1; i < password.length; ++i){
        if(password[i-1] > password[i]){
            return false
        }
        if(password[i] in m){
            m[password[i]] += 1
        }else{
            m[password[i]] = 1
        }
    }

    return m.values().find {it >= 2} != null
}

int c = 0
for(; start <= end; ++start){
    if(isPassword((start as String).collect {Integer.parseInt(it)} as int[])){
        ++c
    }
}

println c
