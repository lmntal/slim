Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 's'_1]
        newatom      [3, 1, 'hoge'_1]
        newatom      [4, 1, 's'_1]
        newatom      [5, 1, 'hoge'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        anymem       [1, 0, 0, null]
        natoms         [1, 4]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 's'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'hoge'_1]
        findatom     [4, 1, 's'_1]
        neqatom        [4, 2]
        deref        [5, 4, 0, 0]
        func           [5, 'hoge'_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removeatom     [5, 1]
        removemem      [1, 0]
        newatom      [6, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        freeatom       [5]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        findatom     [1, 0, 's'_1]
        derefatom    [2, 1, 0]
        isstring       [2]
        commit         ["_ssst", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        newatom      [3, 0, 'yes'_0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


