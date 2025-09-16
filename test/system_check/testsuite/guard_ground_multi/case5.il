Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_3]
        newatom      [3, 1, 'g'_3]
        newlink        [2, 0, 3, 2, 1]
        newlink        [2, 1, 3, 0, 1]
        newlink        [2, 2, 3, 1, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        anymem       [1, 0, 0, null]
        natoms         [1, 2]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'a'_3]
        deref        [5, 2, 2, 1]
        deref        [4, 2, 1, 0]
        deref        [3, 2, 0, 2]
        func           [3, 'g'_3]
        eqatom         [4, 3]
        eqatom         [5, 3]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removemem      [1, 0]
        newatom      [6, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 25]
        findatom     [1, 0, 'a'_3]
        getlink      [4, 1, 2]
        getlink      [3, 1, 1]
        getlink      [2, 1, 0]
        newlist      [5]
        addtolist      [5, 2]
        addtolist      [5, 3]
        newlist      [6]
        addtolist      [6, 4]
        isground     [7, 5, 6, []]
        commit         ["_aXYN", 0]
        newlist      [8]
        getlink      [9, 1, 0]
        addtolist      [8, 9]
        getlink      [10, 1, 1]
        addtolist      [8, 10]
        removeatom     [1, 0]
        removeground   [8, 0]
        insertconnectorsinnull[11, [9, 10]]
        newlist      [12]
        getlink      [13, 1, 0]
        addtolist      [12, 13]
        getlink      [14, 1, 1]
        addtolist      [12, 14]
        copyground   [15, 12, 0, []]
        getfromlist  [16, 15, 0]
        getfromlist  [17, 15, 1]
        newatom      [18, 0, 'b'_3]
        alloclink    [19, 18, 2]
        alloclink    [20, 18, 1]
        alloclink    [21, 18, 0]
        getfromlist  [22, 16, 0]
        getfromlist  [23, 16, 1]
        unifylinks     [21, 22, 0]
        unifylinks     [20, 23, 0]
        getlink      [24, 1, 2]
        unifylinks     [19, 24, 0]
        deleteconnectors[11, 17]
        freeatom       [1]
        freeground     [12]
        proceed        []


