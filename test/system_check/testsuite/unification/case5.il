Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [0, @601]
        newatom      [2, 1, '.'_3]
        newatom      [3, 1, 1_1]
        newatom      [4, 1, '[]'_1]
        newatom      [5, 1, 'res'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [2, 1, 4, 0, 1]
        newlink        [2, 2, 5, 0, 1]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        anymem       [1, 0, 0, null]
        natoms         [1, 4]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'res'_1]
        deref        [3, 2, 0, 2]
        func           [3, '.'_3]
        deref        [5, 3, 1, 0]
        func           [5, '[]'_1]
        deref        [4, 3, 0, 0]
        func           [4, 1_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [5, 1]
        removeatom     [4, 1]
        removemem      [1, 0]
        newatom      [6, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [5]
        freeatom       [4]
        proceed        []


