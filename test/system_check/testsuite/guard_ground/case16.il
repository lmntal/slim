Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_1]
        newatom      [3, 1, 'm'_1]
        newatom      [4, 1, 'b'_1]
        newatom      [5, 1, 'm'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
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
        findatom     [2, 1, 'c'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'm'_1]
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
        spec           [1, 21]
        findatom     [1, 0, 'a'_1]
        getlink      [3, 1, 0]
        findatom     [2, 0, 'b'_1]
        getlink      [4, 2, 0]
        newlist      [5]
        addtolist      [5, 3]
        newlist      [6]
        addtolist      [6, 4]
        isground     [7, 5, 6, []]
        newlist      [8]
        addtolist      [8, 4]
        eqground       [5, 8]
        newlist      [9]
        addtolist      [9, 3]
        isground     [10, 8, 9, []]
        commit         ["_axby", 0]
        newlist      [11]
        getlink      [12, 2, 0]
        addtolist      [11, 12]
        newlist      [13]
        getlink      [14, 1, 0]
        addtolist      [13, 14]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeground   [13, 0]
        removeground   [11, 0]
        copyground   [15, 13, 0, []]
        getfromlist  [16, 15, 0]
        getfromlist  [17, 15, 1]
        newatom      [18, 0, 'c'_1]
        alloclink    [19, 18, 0]
        getfromlist  [20, 16, 0]
        unifylinks     [19, 20, 0]
        freeatom       [1]
        freeatom       [2]
        freeground     [13]
        freeground     [11]
        proceed        []


