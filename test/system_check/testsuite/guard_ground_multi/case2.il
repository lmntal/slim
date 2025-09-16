Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_2]
        newatom      [3, 1, 't'_1]
        newatom      [4, 1, 't'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [2, 1, 4, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        anymem       [1, 0, 0, null]
        natoms         [1, 3]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'a'_2]
        deref        [4, 2, 1, 0]
        func           [4, 't'_1]
        deref        [3, 2, 0, 0]
        func           [3, 't'_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removemem      [1, 0]
        newatom      [5, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 22]
        findatom     [1, 0, 'a'_2]
        getlink      [3, 1, 1]
        getlink      [2, 1, 0]
        newlist      [4]
        addtolist      [4, 2]
        addtolist      [4, 3]
        newlist      [5]
        isground     [6, 4, 5, []]
        commit         ["_aXYx", 0]
        newlist      [7]
        getlink      [8, 1, 0]
        addtolist      [7, 8]
        getlink      [9, 1, 1]
        addtolist      [7, 9]
        removeatom     [1, 0]
        removeground   [7, 0]
        insertconnectorsinnull[10, [8, 9]]
        newlist      [11]
        getlink      [12, 1, 0]
        addtolist      [11, 12]
        getlink      [13, 1, 1]
        addtolist      [11, 13]
        copyground   [14, 11, 0, []]
        getfromlist  [15, 14, 0]
        getfromlist  [16, 14, 1]
        newatom      [17, 0, 'b'_2]
        alloclink    [18, 17, 1]
        alloclink    [19, 17, 0]
        getfromlist  [20, 15, 0]
        getfromlist  [21, 15, 1]
        unifylinks     [19, 20, 0]
        unifylinks     [18, 21, 0]
        deleteconnectors[10, 16]
        freeatom       [1]
        freeground     [11]
        proceed        []


