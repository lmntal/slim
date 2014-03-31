Compiled Ruleset @603 
Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 1]
		jump           [L251, [0], [], []]
	--guard:L251:
		spec           [1, 1]
		jump           [L244, [0], [], []]
	--body:L244:
		spec           [1, 7]
		commit         [null, 0]
		newmem      [1, 0, 0]
		setmemname     [1, "proc_generator"]
		loadruleset    [1, @601]
		loadruleset    [0, @602]
		newatom     [2, 1, 15_1]
		newatom     [3, 1, 'np'_1]
		newatom     [4, 0, 'mutex'_0]
		alloclink   [5, 2, 0]
		alloclink   [6, 3, 0]
		unifylinks     [5, 6, 1]
		enqueueatom    [4]
		enqueueatom    [3]
		proceed        []


Compiled Ruleset @602    
Compiled Rule 
	--atommatch:
		spec           [2, 6]
	--memmatch:
		spec           [1, 4]
		anymem      [1, 0, 0, "proc_generator"]
		stable         [1]
		findatom    [2, 1, 'np'_1]
		deref       [3, 2, 0, 0]
		func           [3, 0_1]
		commit         ["init_end", 0]
		dequeueatom    [2]
		removeatom     [2, 1, 'np'_1]
		removeatom     [3, 1, 0_1]
		removemem      [1, 0]
		removeproxies  [1]
		movecells      [0, 1]
		removetemporaryproxies[0]
		freemem        [1]
		freeatom       [2]
		freeatom       [3]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 3]
	--memmatch:
		spec           [1, 3]
		findatom    [1, 0, 'wait'_1]
		commit         ["request", 0]
		dequeueatom    [1]
		removeatom     [1, 0, 'wait'_1]
		newatom     [2, 0, 'rqst'_1]
		relink         [2, 0, 1, 0, 0]
		enqueueatom    [2]
		freeatom       [1]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 4]
	--memmatch:
		spec           [1, 4]
		findatom    [1, 0, 'rqst'_1]
		findatom    [2, 0, 'mutex'_0]
		commit         ["get_mtx", 0]
		dequeueatom    [1]
		dequeueatom    [2]
		removeatom     [1, 0, 'rqst'_1]
		removeatom     [2, 0, 'mutex'_0]
		newatom     [3, 0, 'crit'_1]
		relink         [3, 0, 1, 0, 0]
		enqueueatom    [3]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 3]
	--memmatch:
		spec           [1, 4]
		findatom    [1, 0, 'crit'_1]
		commit         ["release", 0]
		dequeueatom    [1]
		removeatom     [1, 0, 'crit'_1]
		newatom     [2, 0, 'wait'_1]
		newatom     [3, 0, 'mutex'_0]
		relink         [2, 0, 1, 0, 0]
		enqueueatom    [3]
		enqueueatom    [2]
		freeatom       [1]
		proceed        []


Compiled Ruleset @601 
Compiled Rule 
	--atommatch:
		spec           [2, 3]
	--memmatch:
		spec           [1, 13]
		findatom    [1, 0, 'np'_1]
		derefatom   [3, 1, 0]
		isint          [3]
		allocatom   [2, 0_1]
		igt            [3, 2]
		allocatom   [4, 1_1]
		isub        [5, 3, 4]
		getfunc     [6, 5]
		allocatomindirect[7, 6]
		commit         ["init", 0]
		dequeueatom    [1]
		removeatom     [1, 0, 'np'_1]
		dequeueatom    [3]
		removeatom     [3, 0]
		copyatom    [8, 0, 3]
		copyatom    [9, 0, 7]
		newatom    [10, 0, 'np'_1]
		newatom    [11, 0, 'wait'_1]
		newatom    [12, 0, 'p'_2]
		newlink        [10, 0, 9, 0, 0]
		newlink        [11, 0, 12, 0, 0]
		newlink        [12, 1, 8, 0, 0]
		enqueueatom    [12]
		enqueueatom    [11]
		enqueueatom    [10]
		freeatom       [1]
		freeatom       [2]
		freeatom       [4]
		freeatom       [5]
		freeatom       [3]
		freeatom       [7]
		proceed        []


Inline
