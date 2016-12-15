def main(List<Disk> disks) {
	int time = 0
	while(true){
		println time
		if(disks.every {it.at0(time)}){
			return time	
		}
		++time	
	}
}

@groovy.transform.ToString
class Disk {
	int positions
	int start
	int num

	def at0(int time){
		def at = (start + time + num) % positions 
//		println "Disk $num is at position $at"
		at == 0
	}
}



println(main([
	new Disk(positions: 5, start: 4, num:1),
	new Disk(positions: 2, start: 1, num:2)
]))
println()
println(main([
	new Disk(positions: 17, start: 1, num:1),
	new Disk(positions: 7, start: 0, num:2),
	new Disk(positions: 19, start: 2, num:3),
	new Disk(positions: 5, start: 0, num:4),
	new Disk(positions: 3, start: 0, num:5),
	new Disk(positions: 13, start: 5, num:6),
	new Disk(positions: 11, start: 0, num:7),
]))
