Compiled Ruleset @602 
Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 1]
		jump           [L604, [0], [], []]
	--guard:L604:
		spec           [1, 1]
		jump           [L597, [0], [], []]
	--body:L597:
		spec           [1, 6]
		commit         ["_init", 0]
		newmem       [1, 0, 0]
		loadruleset    [1, @601]
		newatom      [2, 1, 'boolean'_1]
		newatom      [3, 1, 'module'_1]
		alloclink    [4, 2, 0]
		alloclink    [5, 3, 0]
		unifylinks     [4, 5, 1]
		proceed        []


Compiled Ruleset @601              
Compiled Rule 
	--atommatch:
		spec           [2, 6]
	--memmatch:
		spec           [1, 4]
		findatom     [1, 0, 'true'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'not'_2]
		commit         ["_Hnot", 0]
		removeatom     [1, 0, 'true'_1]
		removeatom     [2, 0, 'not'_2]
		newatom      [3, 0, 'false'_1]
		relink         [3, 0, 2, 1, 0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 6]
	--memmatch:
		spec           [1, 4]
		findatom     [1, 0, 'false'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'not'_2]
		commit         ["_Hnot", 0]
		removeatom     [1, 0, 'false'_1]
		removeatom     [2, 0, 'not'_2]
		newatom      [3, 0, 'true'_1]
		relink         [3, 0, 2, 1, 0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 3]
		findatom     [1, 0, 'true'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'and'_3]
		commit         ["_Hand", 0]
		removeatom     [1, 0, 'true'_1]
		removeatom     [2, 0, 'and'_3]
		unify          [2, 2, 2, 1, 0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 3]
		findatom     [1, 0, 'true'_1]
		deref        [2, 1, 0, 1]
		func           [2, 'and'_3]
		commit         ["_Hand", 0]
		removeatom     [1, 0, 'true'_1]
		removeatom     [2, 0, 'and'_3]
		unify          [2, 2, 2, 0, 0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 15]
		findatom     [1, 0, 'false'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'and'_3]
		getlink      [4, 2, 2]
		getlink      [3, 2, 1]
		newlist      [5]
		addtolist      [5, 3]
		newlist      [6]
		addtolist      [6, 4]
		isground     [7, 5, 6]
		commit         ["_Hand", 0]
		newlist      [8]
		getlink      [9, 2, 1]
		addtolist      [8, 9]
		removeatom     [1, 0, 'false'_1]
		removeatom     [2, 0, 'and'_3]
		removeground   [8, 0]
		getlink      [10, 2, 1]
		insertconnectorsinnull[11, [10]]
		newlist      [12]
		getlink      [13, 2, 1]
		addtolist      [12, 13]
		newatom      [14, 0, 'false'_1]
		relink         [14, 0, 2, 2, 0]
		freeatom       [1]
		freeatom       [2]
		freeground     [12]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 15]
		findatom     [1, 0, 'false'_1]
		deref        [2, 1, 0, 1]
		func           [2, 'and'_3]
		getlink      [4, 2, 2]
		getlink      [3, 2, 0]
		newlist      [5]
		addtolist      [5, 3]
		newlist      [6]
		addtolist      [6, 4]
		isground     [7, 5, 6]
		commit         ["_Hand", 0]
		newlist      [8]
		getlink      [9, 2, 0]
		addtolist      [8, 9]
		removeatom     [1, 0, 'false'_1]
		removeatom     [2, 0, 'and'_3]
		removeground   [8, 0]
		getlink      [10, 2, 0]
		insertconnectorsinnull[11, [10]]
		newlist      [12]
		getlink      [13, 2, 0]
		addtolist      [12, 13]
		newatom      [14, 0, 'false'_1]
		relink         [14, 0, 2, 2, 0]
		freeatom       [1]
		freeatom       [2]
		freeground     [12]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 15]
		findatom     [1, 0, 'true'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'or'_3]
		getlink      [4, 2, 2]
		getlink      [3, 2, 1]
		newlist      [5]
		addtolist      [5, 3]
		newlist      [6]
		addtolist      [6, 4]
		isground     [7, 5, 6]
		commit         ["_Hort", 0]
		newlist      [8]
		getlink      [9, 2, 1]
		addtolist      [8, 9]
		removeatom     [1, 0, 'true'_1]
		removeatom     [2, 0, 'or'_3]
		removeground   [8, 0]
		getlink      [10, 2, 1]
		insertconnectorsinnull[11, [10]]
		newlist      [12]
		getlink      [13, 2, 1]
		addtolist      [12, 13]
		newatom      [14, 0, 'true'_1]
		relink         [14, 0, 2, 2, 0]
		freeatom       [1]
		freeatom       [2]
		freeground     [12]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 15]
		findatom     [1, 0, 'true'_1]
		deref        [2, 1, 0, 1]
		func           [2, 'or'_3]
		getlink      [4, 2, 2]
		getlink      [3, 2, 0]
		newlist      [5]
		addtolist      [5, 3]
		newlist      [6]
		addtolist      [6, 4]
		isground     [7, 5, 6]
		commit         ["_Horq", 0]
		newlist      [8]
		getlink      [9, 2, 0]
		addtolist      [8, 9]
		removeatom     [1, 0, 'true'_1]
		removeatom     [2, 0, 'or'_3]
		removeground   [8, 0]
		getlink      [10, 2, 0]
		insertconnectorsinnull[11, [10]]
		newlist      [12]
		getlink      [13, 2, 0]
		addtolist      [12, 13]
		newatom      [14, 0, 'true'_1]
		relink         [14, 0, 2, 2, 0]
		freeatom       [1]
		freeatom       [2]
		freeground     [12]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 3]
		findatom     [1, 0, 'false'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'or'_3]
		commit         ["_Horf", 0]
		removeatom     [1, 0, 'false'_1]
		removeatom     [2, 0, 'or'_3]
		unify          [2, 2, 2, 1, 0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 3]
		findatom     [1, 0, 'false'_1]
		deref        [2, 1, 0, 1]
		func           [2, 'or'_3]
		commit         ["_HorQ", 0]
		removeatom     [1, 0, 'false'_1]
		removeatom     [2, 0, 'or'_3]
		unify          [2, 2, 2, 0, 0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 9]
	--memmatch:
		spec           [1, 5]
		findatom     [1, 0, 'true'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'xor'_3]
		deref        [3, 2, 1, 0]
		func           [3, 'false'_1]
		commit         ["_Hxor", 0]
		removeatom     [1, 0, 'true'_1]
		removeatom     [3, 0, 'false'_1]
		removeatom     [2, 0, 'xor'_3]
		newatom      [4, 0, 'true'_1]
		relink         [4, 0, 2, 2, 0]
		freeatom       [1]
		freeatom       [3]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 9]
	--memmatch:
		spec           [1, 5]
		findatom     [1, 0, 'false'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'xor'_3]
		deref        [3, 2, 1, 0]
		func           [3, 'true'_1]
		commit         ["_Hxor", 0]
		removeatom     [1, 0, 'false'_1]
		removeatom     [3, 0, 'true'_1]
		removeatom     [2, 0, 'xor'_3]
		newatom      [4, 0, 'true'_1]
		relink         [4, 0, 2, 2, 0]
		freeatom       [1]
		freeatom       [3]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 9]
	--memmatch:
		spec           [1, 5]
		findatom     [1, 0, 'false'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'xor'_3]
		deref        [3, 2, 1, 0]
		func           [3, 'false'_1]
		commit         ["_Hxor", 0]
		removeatom     [1, 0, 'false'_1]
		removeatom     [3, 0, 'false'_1]
		removeatom     [2, 0, 'xor'_3]
		newatom      [4, 0, 'false'_1]
		relink         [4, 0, 2, 2, 0]
		freeatom       [1]
		freeatom       [3]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 9]
	--memmatch:
		spec           [1, 5]
		findatom     [1, 0, 'true'_1]
		deref        [2, 1, 0, 0]
		func           [2, 'xor'_3]
		deref        [3, 2, 1, 0]
		func           [3, 'true'_1]
		commit         ["_Hxor", 0]
		removeatom     [1, 0, 'true'_1]
		removeatom     [3, 0, 'true'_1]
		removeatom     [2, 0, 'xor'_3]
		newatom      [4, 0, 'false'_1]
		relink         [4, 0, 2, 2, 0]
		freeatom       [1]
		freeatom       [3]
		freeatom       [2]
		proceed        []


Module
'boolean' {@601}
Inline
