Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'p'_1]
        newatom      [3, 1, 'f'_1]
        newatom      [4, 1, 'p'_1]
        newatom      [5, 1, 'f'_1]
        newatom      [6, 1, 'p'_1]
        newatom      [7, 1, 'f'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
        newlink        [6, 0, 7, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        anymem       [1, 0, 0, null]
        natoms         [1, 2]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'p'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'f'_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removemem      [1, 0]
        newatom      [4, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        findatom     [1, 0, 'p'_1]
        findatom     [2, 0, 'f'_1]
        deref        [3, 2, 0, 0]
        neqatom        [3, 1]
        func           [3, 'p'_1]
        neqatom        [3, 1]
        derefatom    [4, 1, 0]
        isunary        [4]
        commit         ["_pAfX", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [3, 0]
        removeatom     [4, 0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


