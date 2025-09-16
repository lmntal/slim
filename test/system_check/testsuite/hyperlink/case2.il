Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 12]
        newhlink     [1]
        getfunc      [2, 1]
        allocatomindirect[3, 2]
        newhlink     [4]
        getfunc      [5, 4]
        allocatomindirect[6, 5]
        commit         ["_init", 0]
        loadruleset    [0, @601]
        copyatom     [7, 0, 3]
        copyatom     [8, 0, 6]
        newatom      [9, 0, 'leq'_2]
        newatom      [10, 0, 'ok'_0]
        newatom      [11, 0, 'ok'_0]
        newlink        [9, 0, 7, 0, 0]
        newlink        [9, 1, 8, 0, 0]
        freeatom       [3]
        freeatom       [6]
        proceed        []


Compiled Ruleset @601  
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        findproccxt    [1, 2, 0, 1, 2, 1]
        findatom     [1, 0, 'leq'_2]
        derefatom    [3, 1, 1]
        isunary        [3]
        getfunc      [5, 3]
        derefatom    [2, 1, 0]
        ishlink        [2]
        isunary        [2]
        getfunc      [4, 2]
        eqfunc         [4, 5]
        commit         ["_leqX", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [3, 0]
        newatom      [6, 0, 'ng'_0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [3]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 13]
        findatom     [1, 0, 'leq'_2]
        derefatom    [11, 1, 1]
        ishlink        [11]
        derefatom    [10, 1, 0]
        ishlink        [10]
        getlink      [3, 1, 1]
        getlink      [2, 1, 0]
        newlist      [4]
        addtolist      [4, 2]
        newlist      [5]
        addtolist      [5, 3]
        isground     [6, 4, 5, []]
        newlist      [7]
        addtolist      [7, 3]
        neqground      [4, 7]
        newlist      [8]
        addtolist      [8, 2]
        isground     [9, 7, 8, []]
        commit         ["_leqX", 0]
        removeatom     [1, 0]
        removeatom     [10, 0]
        removeatom     [11, 0]
        newatom      [12, 0, 'ok'_0]
        freeatom       [1]
        freeatom       [10]
        freeatom       [11]
        proceed        []


