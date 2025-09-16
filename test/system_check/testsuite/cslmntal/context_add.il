Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 10]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'eval'_1]
        newatom      [3, 1, 'add'_3]
        newatom      [4, 1, 'add'_3]
        newatom      [5, 1, 1_1]
        newatom      [6, 1, 2_1]
        newatom      [7, 1, 'add'_3]
        newatom      [8, 1, 3_1]
        newatom      [9, 1, 4_1]
        newlink        [2, 0, 3, 2, 1]
        newlink        [3, 0, 4, 2, 1]
        newlink        [3, 1, 7, 2, 1]
        newlink        [4, 0, 5, 0, 1]
        newlink        [4, 1, 6, 0, 1]
        newlink        [7, 0, 8, 0, 1]
        newlink        [7, 1, 9, 0, 1]
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
        findatom     [2, 1, 'eval'_1]
        deref        [3, 2, 0, 0]
        func           [3, 10_1]
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
        spec           [1, 25]
        findatom     [1, 0, 'eval'_1]
        getlink      [3, 1, 0]
        findatom     [2, 0, 'add'_3]
        derefatom    [6, 2, 1]
        isint          [6]
        derefatom    [5, 2, 0]
        isint          [5]
        iadd         [7, 5, 6]
        getlink      [4, 2, 2]
        subrule      [8, 0, "context", [0, 4, 3]]
        commit         ["_eval", 0]
        newlist      [9]
        getlink      [10, 2, 2]
        addtolist      [9, 10]
        getlink      [11, 1, 0]
        addtolist      [9, 11]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [5, 0]
        removeatom     [6, 0]
        removeground   [9, 0]
        insertconnectorsinnull[12, [10, 11]]
        newlist      [13]
        getlink      [14, 2, 2]
        addtolist      [13, 14]
        getlink      [15, 1, 0]
        addtolist      [13, 15]
        copyatom     [16, 0, 7]
        copyground   [17, 13, 0, []]
        getfromlist  [18, 17, 0]
        getfromlist  [19, 17, 1]
        newatom      [20, 0, 'eval'_1]
        alloclink    [21, 16, 0]
        alloclink    [22, 20, 0]
        getfromlist  [23, 18, 0]
        getfromlist  [24, 18, 1]
        unifylinks     [22, 24, 0]
        unifylinks     [21, 23, 0]
        deleteconnectors[12, 19]
        freeatom       [1]
        freeatom       [2]
        freeatom       [5]
        freeatom       [6]
        freeatom       [7]
        freeground     [13]
        proceed        []

Compiled Subrule @context
        spec           [3, 4]
        branch         [[
                  subrule      [3, 0, "context_0", [0, 1, 2]]
                  succreturn     [3] ]]
        branch         [[
                  subrule      [3, 0, "context_1", [0, 1, 2]]
                  succreturn     [3] ]]
        branch         [[
                  subrule      [3, 0, "context_2", [0, 1, 2]]
                  succreturn     [3] ]]
        failreturn     []

Compiled Subrule @context_0
        spec           [3, 4]
        ispairedlink   [1, 2]
        allocset     [3]
        succreturn     [3]

Compiled Subrule @context_1
        spec           [3, 8]
        dereflink    [3, 2, 2]
        func           [3, 'add'_3]
        derefatom    [5, 3, 0]
        isint          [5]
        getlink      [4, 3, 1]
        subrule      [6, 0, "context", [0, 1, 4]]
        allocset     [7]
        succreturn     [7]

Compiled Subrule @context_2
        spec           [3, 11]
        dereflink    [3, 2, 2]
        func           [3, 'add'_3]
        getlink      [5, 3, 1]
        getlink      [4, 3, 0]
        subrule      [6, 0, "context", [0, 1, 4]]
        newlist      [7]
        addtolist      [7, 5]
        newlist      [8]
        addtolist      [8, 1]
        addtolist      [8, 4]
        isground     [9, 7, 8, []]
        allocset     [10]
        succreturn     [10]


