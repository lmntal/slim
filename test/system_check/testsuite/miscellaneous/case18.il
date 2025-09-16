Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'e'_1]
        newatom      [3, 1, 'p'_5]
        newlink        [2, 0, 3, 0, 1]
        newlink        [3, 1, 3, 2, 1]
        newlink        [3, 3, 3, 4, 1]
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
        findatom     [2, 1, 'e'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'g'_1]
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
        spec           [1, 3]
        findatom     [1, 0, 'p'_5]
        commit         ["_pABC", 0]
        removeatom     [1, 0]
        unify          [1, 0, 1, 1, 0]
        unify          [1, 2, 1, 3, 0]
        newatom      [2, 0, 'g'_1]
        relink         [2, 0, 1, 4, 0]
        freeatom       [1]
        proceed        []


