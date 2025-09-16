Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'f'_2]
        newatom      [3, 1, 'f'_2]
        newatom      [4, 1, 'f'_2]
        newatom      [5, 1, 'f'_2]
        newatom      [6, 1, 'c'_0]
        newatom      [7, 1, 'c'_0]
        newlink        [2, 0, 3, 1, 1]
        newlink        [2, 1, 5, 0, 1]
        newlink        [3, 0, 4, 1, 1]
        newlink        [4, 0, 5, 1, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        anymem       [1, 0, 0, null]
        natoms         [1, 4]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'f'_2]
        deref        [4, 2, 1, 0]
        func           [4, 'f'_2]
        deref        [6, 4, 1, 0]
        deref        [3, 2, 0, 1]
        func           [3, 'g'_2]
        deref        [5, 3, 0, 1]
        func           [5, 'g'_2]
        eqatom         [6, 5]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [5, 1]
        removeatom     [4, 1]
        removemem      [1, 0]
        newatom      [7, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [5]
        freeatom       [4]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        findatom     [1, 0, 'c'_0]
        findatom     [2, 0, 'f'_2]
        commit         ["_cXfY", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        newatom      [3, 0, 'g'_2]
        relink         [3, 0, 2, 0, 0]
        relink         [3, 1, 2, 1, 0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


