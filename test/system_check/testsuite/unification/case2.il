Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [0, @601]
        newatom      [2, 1, 'p'_2]
        newlink        [2, 0, 2, 1, 1]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        anymem       [1, 0, 0, null]
        natoms         [1, 1]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'p'_2]
        deref        [3, 2, 0, 1]
        eqatom         [3, 2]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removemem      [1, 0]
        newatom      [4, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        proceed        []


