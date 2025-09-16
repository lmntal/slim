Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 14]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'list'_1]
        newatom      [3, 1, '.'_3]
        newatom      [4, 1, 1_1]
        newatom      [5, 1, '.'_3]
        newatom      [6, 1, 2_1]
        newatom      [7, 1, '.'_3]
        newatom      [8, 1, 3_1]
        newatom      [9, 1, '.'_3]
        newatom      [10, 1, 4_1]
        newatom      [11, 1, '.'_3]
        newatom      [12, 1, 5_1]
        newatom      [13, 1, '[]'_1]
        newlink        [2, 0, 3, 2, 1]
        newlink        [3, 0, 4, 0, 1]
        newlink        [3, 1, 5, 2, 1]
        newlink        [5, 0, 6, 0, 1]
        newlink        [5, 1, 7, 2, 1]
        newlink        [7, 0, 8, 0, 1]
        newlink        [7, 1, 9, 2, 1]
        newlink        [9, 0, 10, 0, 1]
        newlink        [9, 1, 11, 2, 1]
        newlink        [11, 0, 12, 0, 1]
        newlink        [11, 1, 13, 0, 1]
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
        findatom     [2, 1, 'list'_1]
        deref        [3, 2, 0, 0]
        func           [3, '[]'_1]
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
        spec           [1, 14]
        findatom     [1, 0, 'list'_1]
        deref        [2, 1, 0, 2]
        func           [2, '.'_3]
        derefatom    [5, 2, 0]
        isint          [5]
        getlink      [3, 2, 1]
        subrule      [4, 0, "dlist", [0, 3]]
        commit         ["rmvhead", 0]
        newlist      [6]
        getlink      [7, 2, 1]
        addtolist      [6, 7]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeground   [6, 0]
        removeatom     [5, 0]
        copyground   [8, 6, 0, []]
        getfromlist  [9, 8, 0]
        getfromlist  [10, 8, 1]
        newatom      [11, 0, 'list'_1]
        alloclink    [12, 11, 0]
        getfromlist  [13, 9, 0]
        unifylinks     [12, 13, 0]
        freeatom       [1]
        freeatom       [2]
        freeground     [6]
        freeatom       [5]
        proceed        []

Compiled Subrule @dlist
        spec           [2, 3]
        branch         [[
                  subrule      [2, 0, "dlist_0", [0, 1]]
                  succreturn     [2] ]]
        branch         [[
                  subrule      [2, 0, "dlist_1", [0, 1]]
                  succreturn     [2] ]]
        failreturn     []

Compiled Subrule @dlist_0
        spec           [2, 4]
        dereflink    [2, 1, 0]
        func           [2, '[]'_1]
        allocset     [3]
        succreturn     [3]

Compiled Subrule @dlist_1
        spec           [2, 7]
        dereflink    [2, 1, 2]
        func           [2, '.'_3]
        derefatom    [5, 2, 0]
        isunary        [5]
        getlink      [3, 2, 1]
        subrule      [4, 0, "dlist", [0, 3]]
        allocset     [6]
        succreturn     [6]


