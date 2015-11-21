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
		newatom      [2, 1, 'float'_1]
		newatom      [3, 1, 'module'_1]
		alloclink    [4, 2, 0]
		alloclink    [5, 3, 0]
		unifylinks     [4, 5, 1]
		proceed        []


Compiled Ruleset @601 
Compiled Rule 
	--atommatch:
		spec           [2, 4]
	--memmatch:
		spec           [1, 6]
		findatom     [1, 0, 'float'.'of_str'_2]
		derefatom    [2, 1, 0]
		isstring       [2]
		commit         ["_Hflo", 0]
		removeatom     [1, 0, 'float'.'of_str'_2]
		removeatom     [2, 0]
		copyatom     [3, 0, 2]
		newatom      [4, 0, 'float_of_string'_1]
		newatom      [5, 0, '$callback'_3]
		newlink        [4, 0, 5, 0, 0]
		newlink        [5, 1, 3, 0, 0]
		relink         [5, 2, 1, 1, 0]
		callback       [0, 5]
		freeatom       [1]
		freeatom       [2]
		proceed        []


Module
'float' {@601}
Inline
