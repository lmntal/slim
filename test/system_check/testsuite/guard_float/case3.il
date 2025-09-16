Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'sum'_3]
        newatom      [3, 1, 21.0_1]
        newatom      [4, 1, 24.2_1]
        newatom      [5, 1, 5.3_1]
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
        func           [3, 50.5_1]
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
        spec           [1, 9]
        findatom     [1, 0, 'sum'_3]
        derefatom    [5, 1, 2]
        isfloat        [5]
        derefatom    [3, 1, 1]
        isfloat        [3]
        derefatom    [2, 1, 0]
        isfloat        [2]
        fadd         [4, 2, 3]
        fadd         [6, 4, 5]
        commit         ["_sumi", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [3, 0]
        removeatom     [5, 0]
        copyatom     [7, 0, 6]
        newatom      [8, 0, 'ans'_1]
        newlink        [8, 0, 7, 0, 0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [6]
        freeatom       [3]
        freeatom       [5]
        freeatom       [4]
        proceed        []


