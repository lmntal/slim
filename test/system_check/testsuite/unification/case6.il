Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [0, @601]
        newatom      [2, 1, '.'_3]
        newatom      [3, 1, 1_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [2, 1, 2, 2, 1]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        anymem       [1, 0, 0, null]
        natoms         [1, 2]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, '.'_3]
        deref        [4, 2, 1, 2]
        eqatom         [4, 2]
        deref        [3, 2, 0, 0]
        func           [3, 1_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removemem      [1, 0]
        newatom      [5, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        proceed        []


