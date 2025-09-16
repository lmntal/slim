Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'i'_1]
        newatom      [3, 1, 3_1]
        newatom      [4, 1, 'i'_1]
        newatom      [5, 1, 4_1]
        newatom      [6, 1, 'i'_1]
        newatom      [7, 1, 5_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
        newlink        [6, 0, 7, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        anymem       [1, 0, 0, null]
        natoms         [1, 6]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'j'_1]
        deref        [3, 2, 0, 0]
        func           [3, 3_1]
        findatom     [4, 1, 'j'_1]
        neqatom        [4, 2]
        deref        [5, 4, 0, 0]
        func           [5, 4_1]
        findatom     [6, 1, 'j'_1]
        neqatom        [6, 2]
        neqatom        [6, 4]
        deref        [7, 6, 0, 0]
        func           [7, 5_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [4, 1]
        removeatom     [6, 1]
        removeatom     [3, 1]
        removeatom     [5, 1]
        removeatom     [7, 1]
        removemem      [1, 0]
        newatom      [8, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [4]
        freeatom       [6]
        freeatom       [3]
        freeatom       [5]
        freeatom       [7]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        findatom     [1, 0, 'i'_1]
        derefatom    [2, 1, 0]
        isint          [2]
        commit         ["_iiin", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        copyatom     [3, 0, 2]
        newatom      [4, 0, 'j'_1]
        newlink        [4, 0, 3, 0, 0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


