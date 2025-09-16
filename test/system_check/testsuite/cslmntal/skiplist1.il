Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'skiplistp'_2]
        newatom      [3, 1, 'a'_4]
        newatom      [4, 1, 'b'_2]
        newatom      [5, 1, 'b'_2]
        newatom      [6, 1, 'a'_4]
        newatom      [7, 1, 'a'_4]
        newatom      [8, 1, 'nil'_2]
        newlink        [2, 0, 3, 3, 1]
        newlink        [2, 1, 3, 2, 1]
        newlink        [3, 0, 4, 1, 1]
        newlink        [3, 1, 6, 2, 1]
        newlink        [4, 0, 5, 1, 1]
        newlink        [5, 0, 6, 3, 1]
        newlink        [6, 0, 7, 3, 1]
        newlink        [6, 1, 7, 2, 1]
        newlink        [7, 0, 8, 1, 1]
        newlink        [7, 1, 8, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        anymem       [1, 0, 0, null]
        natoms         [1, 1]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'yes'_0]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removemem      [1, 0]
        newatom      [3, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        proceed        []


Compiled Ruleset @601     
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        findatom     [1, 0, 'skiplistp'_2]
        getlink      [3, 1, 1]
        getlink      [2, 1, 0]
        subrule      [4, 0, "skiplist", [0, 3, 2]]
        commit         ["isSkiplist", 0]
        newlist      [5]
        getlink      [6, 1, 1]
        addtolist      [5, 6]
        getlink      [7, 1, 0]
        addtolist      [5, 7]
        removeatom     [1, 0]
        removeground   [5, 0]
        newatom      [8, 0, 'yes'_0]
        freeatom       [1]
        freeground     [5]
        proceed        []

Compiled Subrule @skiplist
        spec           [3, 4]
        branch         [[
                  subrule      [3, 0, "skiplist_0", [0, 1, 2]]
                  succreturn     [3] ]]
        branch         [[
                  subrule      [3, 0, "skiplist_1", [0, 1, 2]]
                  succreturn     [3] ]]
        branch         [[
                  subrule      [3, 0, "skiplist_2", [0, 1, 2]]
                  succreturn     [3] ]]
        failreturn     []

Compiled Subrule @skiplist_0
        spec           [3, 6]
        dereflink    [3, 1, 0]
        func           [3, 'nil'_2]
        dereflink    [4, 2, 1]
        eqatom         [4, 3]
        allocset     [5]
        succreturn     [5]

Compiled Subrule @skiplist_1
        spec           [3, 9]
        dereflink    [3, 1, 2]
        func           [3, 'a'_4]
        dereflink    [4, 2, 3]
        eqatom         [4, 3]
        getlink      [6, 3, 1]
        getlink      [5, 3, 0]
        subrule      [7, 0, "skiplist", [0, 6, 5]]
        allocset     [8]
        succreturn     [8]

Compiled Subrule @skiplist_2
        spec           [3, 7]
        dereflink    [3, 2, 1]
        func           [3, 'b'_2]
        getlink      [4, 3, 0]
        subrule      [5, 0, "skiplist", [0, 1, 4]]
        allocset     [6]
        succreturn     [6]


