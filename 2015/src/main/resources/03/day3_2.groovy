def text = new File('day3.data').text

def visited = [[0,0]] as Set

cur = [0,0]
text.eachWithIndex { it, index ->
    if(index % 2 == 0){
	    switch(it){
		    case '>':
		    	cur = [cur[0]+1, cur[1]]; break
		    case '<':
		        cur = [cur[0]-1, cur[1]]; break
		    case '^':
    			cur = [cur[0], cur[1]-1]; break
	    	case 'v':
		    	cur = [cur[0], cur[1]+1]; break
	    }
	    visited << cur 
    }
}

cur = [0,0]
text.eachWithIndex { it, index ->
    if(index % 2 == 1){
	    switch(it){
		    case '>':
		    	cur = [cur[0]+1, cur[1]]; break
		    case '<':
		        cur = [cur[0]-1, cur[1]]; break
		    case '^':
    			cur = [cur[0], cur[1]-1]; break
	    	case 'v':
		    	cur = [cur[0], cur[1]+1]; break
	    }
	    visited << cur 
    }
}

println visited.size()
//println visited
