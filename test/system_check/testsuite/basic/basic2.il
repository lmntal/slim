Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_2]
        newatom      [3, 1, 'f'_1]
        newatom      [4, 1, 'f'_1]
        newatom      [5, 1, 'a'_2]
        newatom      [6, 1, 'f'_1]
        newatom      [7, 1, 'f'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [2, 1, 4, 0, 1]
        newlink        [5, 0, 6, 0, 1]
        newlink        [5, 1, 7, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        anymem       [1, 0, 0, null]
        natoms         [1, 3]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'end'_2]
        deref        [4, 2, 1, 0]
        func           [4, 'f'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'f'_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removemem      [1, 0]
        newatom      [5, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        findatom     [1, 0, 'f'_1]
        deref        [2, 1, 0, 0]
        func           [2, 'a'_2]
        findatom     [3, 0, 'f'_1]
        neqatom        [3, 1]
        deref        [4, 3, 0, 0]
        func           [4, 'a'_2]
        neqatom        [4, 2]
        commit         ["_fAaA", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [3, 0]
        removeatom     [4, 0]
        newatom      [5, 0, 'end'_2]
        relink         [5, 0, 2, 1, 0]
        relink         [5, 1, 4, 1, 0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


