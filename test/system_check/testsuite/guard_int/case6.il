Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'msum'_3]
        newatom      [3, 1, 4_1]
        newatom      [4, 1, 8_1]
        newatom      [5, 1, 12_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [2, 1, 4, 0, 1]
        newlink        [2, 2, 5, 0, 1]
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
        findatom     [2, 1, 'ans'_1]
        deref        [3, 2, 0, 0]
        func           [3, -24_1]
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
        spec           [1, 10]
        findatom     [1, 0, 'msum'_3]
        derefatom    [6, 1, 2]
        isint          [6]
        derefatom    [4, 1, 1]
        isint          [4]
        derefatom    [2, 1, 0]
        isint          [2]
        ineg         [3, 2]
        isub         [5, 3, 4]
        isub         [7, 5, 6]
        commit         ["_msum", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [4, 0]
        removeatom     [6, 0]
        copyatom     [8, 0, 7]
        newatom      [9, 0, 'ans'_1]
        newlink        [9, 0, 8, 0, 0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [7]
        freeatom       [4]
        freeatom       [6]
        freeatom       [5]
        freeatom       [3]
        proceed        []


