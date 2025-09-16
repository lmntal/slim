Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_1]
        newatom      [3, 1, 3_1]
        newatom      [4, 1, 'a'_1]
        newatom      [5, 1, 'g'_1]
        newatom      [6, 1, 'a'_1]
        newatom      [7, 1, 't'_2]
        newatom      [8, 1, 't'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
        newlink        [6, 0, 7, 1, 1]
        newlink        [7, 0, 8, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 10]
        anymem       [1, 0, 0, null]
        natoms         [1, 7]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'b'_1]
        deref        [3, 2, 0, 0]
        func           [3, 3_1]
        findatom     [4, 1, 'a'_1]
        deref        [5, 4, 0, 0]
        func           [5, 'g'_1]
        findatom     [6, 1, 'a'_1]
        neqatom        [6, 4]
        deref        [7, 6, 0, 1]
        func           [7, 't'_2]
        deref        [8, 7, 0, 0]
        func           [8, 't'_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [4, 1]
        removeatom     [5, 1]
        removeatom     [6, 1]
        removeatom     [7, 1]
        removeatom     [8, 1]
        removeatom     [3, 1]
        removemem      [1, 0]
        newatom      [9, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [4]
        freeatom       [5]
        freeatom       [6]
        freeatom       [7]
        freeatom       [8]
        freeatom       [3]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        findatom     [1, 0, 'a'_1]
        derefatom    [2, 1, 0]
        isint          [2]
        isunary        [2]
        commit         ["_axin", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        copyatom     [3, 0, 2]
        newatom      [4, 0, 'b'_1]
        newlink        [4, 0, 3, 0, 0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


