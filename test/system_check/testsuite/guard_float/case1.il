Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 12]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'f'_1]
        newatom      [3, 1, 0.0_1]
        newatom      [4, 1, 'f'_1]
        newatom      [5, 1, -0.0_1]
        newatom      [6, 1, 'f'_1]
        newatom      [7, 1, 1.0_1]
        newatom      [8, 1, 'f'_1]
        newatom      [9, 1, 2.2_1]
        newatom      [10, 1, 'f'_1]
        newatom      [11, 1, -3.5_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
        newlink        [6, 0, 7, 0, 1]
        newlink        [8, 0, 9, 0, 1]
        newlink        [10, 0, 11, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        anymem       [1, 0, 0, null]
        natoms         [1, 5]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'yes'_0]
        findatom     [3, 1, 'yes'_0]
        neqatom        [3, 2]
        findatom     [4, 1, 'yes'_0]
        neqatom        [4, 2]
        neqatom        [4, 3]
        findatom     [5, 1, 'yes'_0]
        neqatom        [5, 2]
        neqatom        [5, 3]
        neqatom        [5, 4]
        findatom     [6, 1, 'yes'_0]
        neqatom        [6, 2]
        neqatom        [6, 3]
        neqatom        [6, 4]
        neqatom        [6, 5]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removeatom     [5, 1]
        removeatom     [6, 1]
        removemem      [1, 0]
        newatom      [7, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        freeatom       [5]
        freeatom       [6]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        findatom     [1, 0, 'f'_1]
        derefatom    [2, 1, 0]
        isfloat        [2]
        commit         ["_fffl", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        newatom      [3, 0, 'yes'_0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


