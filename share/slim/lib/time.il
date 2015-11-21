Compiled Ruleset @602 
Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 1]
		jump           [L135, [0], [], []]
	--guard:L135:
		spec           [1, 1]
		jump           [L128, [0], [], []]
	--body:L128:
		spec           [1, 6]
		commit         ["_init", 0]
		newmem       [1, 0, 0]
		loadruleset    [1, @601]
		newatom      [2, 1, 'time'_1]
		newatom      [3, 1, 'module'_1]
		alloclink    [4, 2, 0]
		alloclink    [5, 3, 0]
		unifylinks     [4, 5, 1]
		proceed        []


Compiled Ruleset @601 
Compiled Rule 
	--atommatch:
		spec           [2, 3]
	--memmatch:
		spec           [1, 4]
		findatom     [1, 0, 'time'.'gettime'_1]
		commit         ["_Htim", 0]
		removeatom     [1, 0, 'time'.'gettime'_1]
		newatom      [2, 0, 'gettime'_1]
		newatom      [3, 0, '$callback'_2]
		newlink        [2, 0, 3, 0, 0]
		relink         [3, 1, 1, 0, 0]
		callback       [0, 3]
		freeatom       [1]
		proceed        []


Module
'time' {@601}
Inline
