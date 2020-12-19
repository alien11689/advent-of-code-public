class Boss {
	int hitPoints = 55
	int damage = 8
}

spells = ['Magic missle':53 , 'Drain': 73, 'Shield': 113, 'Poison': 173, 'Recharge': 229]

class Hero {
	int hitPoints = 50
	int mana = 500	
	Map<String, Integer> currentEffects = [:]
	List<String> used = []
	int armor
}
	
	def fight(hero){
		def boss = new Boss()
		def current = hero
		
		while(true){
			if(current == hero){
				hero.hitPoints -= 1
				if(hero.hitPoints <= 0){
					return false
				}
				hero.armor = 0
				applyEffects(hero, boss)
				if(boss.hitPoints <= 0){
					return true
				}
				def spell = chooseSpell(hero.mana, hero.currentEffects.keySet())
				if(!spell){
					return false	
				}
				hero.used << spell
				applySpell(spell, hero, boss)
				if(boss.hitPoints <= 0){
					return true
				}
				
				current = boss	
			}else {
				hero.armor = 0
				applyEffects(hero, boss)
				if(boss.hitPoints <= 0){
					return true
				}
				int damage = boss.damage - hero.armor
				hero.hitPoints -= ([1, boss.damage - hero.armor].max())
				if(hero.hitPoints <= 0){
					return false
				}
				current = hero
			}
		}
	}

def applyEffects(hero, boss){
	def effects = []
	def newEffects = [:]
	hero.currentEffects.each {
		effects << it.key
		newEffects[it.key] = it.value - 1
	}
	hero.currentEffects = newEffects.findAll {it.value > 0}
	effects.each {
		if(it == 'Shield'){
			hero.armor = 7	
		}else if (it == 'Poison'){
			boss.hitPoints -= 3	
		}else if (it == 'Recharge'){
			hero.mana += 101
		}
	}

}

def chooseSpell(mana, currentEffects){
	 def available = spells.findAll {it.value <= mana && !(it.key in currentEffects)}.collect {it.key}
	 if(!available){
		return null
	 }
	 return available[new Random().nextInt() % available.size()]
}

def applySpell(spell, hero, boss){
	def mana = spells[spell]
	hero.mana -= mana
	if(spell == 'Magic missle'){
		boss.hitPoints -= 4	
	}else if(spell == 'Drain'){
		boss.hitPoints -= 2
		hero.hitPoints += 2
	}else if(spell == 'Shield'){
		hero.currentEffects['Shield'] = 6
	}else if(spell == 'Poison'){
		hero.currentEffects['Poison'] = 6
	}else if(spell == 'Recharge'){
		hero.currentEffects['Recharge'] = 5
	}
}


def calculateCost(spellList){
	spellList.collect { spells[it] }.sum()	
}

best = []
bestMana = 1000000000

(1..100000).each {
	def hero = new Hero()
	if(fight(hero)){
		println 'win'
		List<String> used = hero.used
		def cost = calculateCost(used)
		if(cost <= bestMana){
			best = used
			bestMana = cost	
		}
	}
	println "Iteration $it: $best => $bestMana"
}
