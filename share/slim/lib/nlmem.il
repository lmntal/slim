Compiled Ruleset @602 
Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 1]
		jump           [L234, [0], [], []]
	--guard:L234:
		spec           [1, 1]
		jump           [L227, [0], [], []]
	--body:L227:
		spec           [1, 6]
		commit         ["_init", 0]
		newmem       [1, 0, 0]
		loadruleset    [1, @601]
		newatom      [2, 1, 'nlmem'_1]
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
		spec           [1, 4]
		findatom     [1, 0, 'nlmem'.'copy'_2]
		commit         ["_Rnlm", 0]
		removeatom     [1, 0, 'nlmem'.'copy'_2]
		newatom      [2, 0, 'copied'_1]
		newatom      [3, 0, 'nlmem'.'copy'_3]
		newlink        [2, 0, 3, 1, 0]
		relink         [3, 0, 1, 0, 0]
		relink         [3, 2, 1, 1, 0]
		loadmodule     [0, "nlmem"]
		freeatom       [1]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 5]
	--memmatch:
		spec           [1, 6]
		findatom     [1, 0, 'nlmem'.'copy'_3]
		derefatom    [2, 1, 1]
		isunary        [2]
		commit         ["_nlme", 0]
		removeatom     [1, 0, 'nlmem'.'copy'_3]
		removeatom     [2, 0]
		copyatom     [3, 0, 2]
		newatom      [4, 0, 'nlmem_copy'_1]
		newatom      [5, 0, '$callback'_4]
		newlink        [4, 0, 5, 0, 0]
		relink         [5, 1, 1, 0, 0]
		newlink        [5, 2, 3, 0, 0]
		relink         [5, 3, 1, 2, 0]
		callback       [0, 5]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 12]
	--memmatch:
		spec           [1, 12]
		findatom     [1, 0, 'nlmem'.'kill'_1]
		deref        [2, 1, 0, 1]
		func           [2, $out_2]
		deref        [3, 2, 0, 0]
		func           [3, $in_2]
		deref        [5, 3, 1, 0]
		func           [5, '+'_1]
		lockmem      [4, 3, null]
		commit         ["_nlme", 0]
		removeatom     [1, 0, 'nlmem'.'kill'_1]
		removeatom     [2, 0, $out_2]
		removeatom     [5, 4, '+'_1]
		removeatom     [3, 4, $in_2]
		removeproxies  [4]
		enqueuemem     [4]
		insertproxies  [0, 4]
		newatom      [7, 4, '+'_1]
		newatom      [8, 4, $in_2]
		newatom      [9, 0, 'killed'_1]
		newatom      [10, 0, 'nlmem'.'kill'_2]
		newatom      [11, 0, $out_2]
		newlink        [7, 0, 8, 1, 4]
		newlink        [9, 0, 10, 1, 0]
		newlink        [10, 0, 11, 1, 0]
		newlink        [11, 0, 8, 0, 0]
		loadmodule     [0, "nlmem"]
		freeatom       [1]
		freeatom       [2]
		freeatom       [5]
		freeatom       [3]
		unlockmem      [4]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 4]
	--memmatch:
		spec           [1, 6]
		findatom     [1, 0, 'nlmem'.'kill'_2]
		derefatom    [2, 1, 1]
		isunary        [2]
		commit         ["_nlme", 0]
		removeatom     [1, 0, 'nlmem'.'kill'_2]
		removeatom     [2, 0]
		copyatom     [3, 0, 2]
		newatom      [4, 0, 'nlmem_kill'_1]
		newatom      [5, 0, '$callback'_3]
		newlink        [4, 0, 5, 0, 0]
		relink         [5, 1, 1, 0, 0]
		newlink        [5, 2, 3, 0, 0]
		callback       [0, 5]
		freeatom       [1]
		freeatom       [2]
		proceed        []


Module
'nlmem' {@601}
Inline
