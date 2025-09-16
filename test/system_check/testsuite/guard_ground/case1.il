Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_1]
        newatom      [3, 1, 'n'_1]
        newlink        [2, 0, 3, 0, 1]
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
        findatom     [2, 1, 'b'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'n'_1]
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
        findatom     [1, 0, 'a'_1]
        getlink      [2, 1, 0]
        newlist      [3]
        addtolist      [3, 2]
        newlist      [4]
        isground     [5, 3, 4, []]
        commit         ["_axgr", 0]
        newlist      [6]
        getlink      [7, 1, 0]
        addtolist      [6, 7]
        removeatom     [1, 0]
        removeground   [6, 0]
        copyground   [8, 6, 0, []]
        getfromlist  [9, 8, 0]
        getfromlist  [10, 8, 1]
        newatom      [11, 0, 'b'_1]
        alloclink    [12, 11, 0]
        getfromlist  [13, 9, 0]
        unifylinks     [12, 13, 0]
        freeatom       [1]
        freeground     [6]
        proceed        []


