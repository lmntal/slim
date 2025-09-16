Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 10]
        newhlink     [1]
        getfunc      [2, 1]
        allocatomindirect[3, 2]
        commit         ["_init", 0]
        loadruleset    [0, @601]
        copyatom     [4, 0, 3]
        copyatom     [5, 0, 3]
        newatom      [6, 0, 'a'_1]
        newatom      [7, 0, 'b'_1]
        newatom      [8, 0, 'ok'_0]
        newatom      [9, 0, 'ok'_0]
        newlink        [6, 0, 4, 0, 0]
        newlink        [7, 0, 5, 0, 0]
        freeatom       [3]
        proceed        []


Compiled Ruleset @601  
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        findproccxt    [1, 1, 0, 2, 1, 0]
        findatom     [1, 0, 'a'_1]
        findatom     [2, 0, 'b'_1]
        derefatom    [4, 2, 0]
        isunary        [4]
        getfunc      [6, 4]
        derefatom    [3, 1, 0]
        ishlink        [3]
        isunary        [3]
        getfunc      [5, 3]
        eqfunc         [5, 6]
        commit         ["_aTbT", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [3, 0]
        removeatom     [4, 0]
        copyatom     [7, 0, 3]
        newatom      [8, 0, 'x'_1]
        newlink        [8, 0, 7, 0, 0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        findatom     [1, 0, 'x'_1]
        derefatom    [2, 1, 0]
        ishlink        [2]
        commit         ["_xHhl", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        newatom      [3, 0, 'ok'_0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


