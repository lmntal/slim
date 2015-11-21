Compiled Ruleset @602 
Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 1]
		jump           [L226, [0], [], []]
	--guard:L226:
		spec           [1, 1]
		jump           [L219, [0], [], []]
	--body:L219:
		spec           [1, 6]
		commit         ["_init", 0]
		newmem       [1, 0, 0]
		loadruleset    [1, @601]
		newatom      [2, 1, 'nd_conf'_1]
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
		anymem       [1, 0, 0, "nd_conf"]
		nfreelinks     [1, 0]
		findatom     [2, 1, 'atom_priority'_1]
		commit         ["_nd_c", 0]
		removeatom     [2, 1, 'atom_priority'_1]
		removeproxies  [1]
		enqueuemem     [1]
		setmemname     [1, "nd_conf"]
		insertproxies  [0, 1]
		newatom      [4, 1, 1_1]
		newatom      [5, 1, 'atom_priority'_2]
		newlink        [4, 0, 5, 0, 1]
		relink         [5, 1, 2, 0, 1]
		freeatom       [2]
		unlockmem      [1]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 7]
	--memmatch:
		spec           [1, 6]
		anymem       [1, 0, 0, "nd_conf"]
		findatom     [2, 1, 'atom_priority'_2]
		derefatom    [4, 2, 0]
		isint          [4]
		deref        [3, 2, 1, 0]
		func           [3, '[]'_1]
		commit         ["_nd_c", 0]
		removeatom     [2, 1, 'atom_priority'_2]
		removeatom     [3, 1, '[]'_1]
		removeatom     [4, 1]
		removeproxies  [1]
		enqueuemem     [1]
		setmemname     [1, "nd_conf"]
		insertproxies  [0, 1]
		freeatom       [2]
		freeatom       [3]
		freeatom       [4]
		unlockmem      [1]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 13]
	--memmatch:
		spec           [1, 19]
		anymem       [1, 0, 0, "nd_conf"]
		nfreelinks     [1, 0]
		findatom     [2, 1, '/'_3]
		derefatom    [10, 2, 0]
		isunary        [10]
		derefatom    [6, 2, 1]
		isint          [6]
		allocatom    [5, 0_1]
		ige            [6, 5]
		deref        [3, 2, 2, 0]
		func           [3, '.'_3]
		deref        [4, 3, 2, 1]
		func           [4, 'atom_priority'_2]
		derefatom    [8, 4, 0]
		isint          [8]
		allocatom    [7, 1_1]
		iadd         [9, 8, 7]
		commit         ["_nd_c", 0]
		removeatom     [2, 1, '/'_3]
		removeatom     [4, 1, 'atom_priority'_2]
		removeatom     [3, 1, '.'_3]
		removeatom     [10, 1]
		removeatom     [6, 1]
		removeatom     [8, 1]
		removeproxies  [1]
		enqueuemem     [1]
		setmemname     [1, "nd_conf"]
		insertproxies  [0, 1]
		copyatom     [12, 0, 10]
		copyatom     [13, 0, 6]
		copyatom     [14, 1, 9]
		copyatom     [15, 0, 8]
		newatom      [16, 1, 'atom_priority'_2]
		newatom      [17, 0, 'set_functor_priority'_1]
		newatom      [18, 0, '$callback'_4]
		newlink        [16, 0, 14, 0, 1]
		relink         [16, 1, 3, 1, 1]
		newlink        [17, 0, 18, 0, 0]
		newlink        [18, 1, 15, 0, 0]
		newlink        [18, 2, 12, 0, 0]
		newlink        [18, 3, 13, 0, 0]
		callback       [0, 18]
		freeatom       [2]
		freeatom       [4]
		freeatom       [3]
		freeatom       [10]
		freeatom       [6]
		freeatom       [9]
		freeatom       [8]
		freeatom       [7]
		freeatom       [5]
		unlockmem      [1]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 2]
		anymem       [1, 0, 0, "nd_conf"]
		natoms         [1, 0]
		nmems          [1, 0]
		commit         ["_nd_c", 0]
		removemem      [1, 0]
		freemem        [1]
		proceed        []


Module
'nd_conf' {@601}
Inline
