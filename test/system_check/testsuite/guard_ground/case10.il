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
        newatom      [3, 1, 'n'_1]
        newatom      [4, 1, 'n'_1]
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
        findatom     [2, 1, 'b'_2]
        deref        [4, 2, 1, 0]
        func           [4, 'n'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'n'_1]
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
        spec           [1, 17]
        findatom     [1, 0, 'a'_2]
        getlink      [3, 1, 1]
        getlink      [2, 1, 0]
        newlist      [4]
        addtolist      [4, 2]
        newlist      [5]
        addtolist      [5, 3]
        isground     [6, 4, 5, []]
        commit         ["_axYg", 0]
        newlist      [7]
        getlink      [8, 1, 0]
        addtolist      [7, 8]
        removeatom     [1, 0]
        removeground   [7, 0]
        copyground   [9, 7, 0, []]
        getfromlist  [10, 9, 0]
        getfromlist  [11, 9, 1]
        newatom      [12, 0, 'b'_2]
        alloclink    [13, 12, 1]
        alloclink    [14, 12, 0]
        getfromlist  [15, 10, 0]
        unifylinks     [14, 15, 0]
        getlink      [16, 1, 1]
        unifylinks     [13, 16, 0]
        freeatom       [1]
        freeground     [7]
        proceed        []


