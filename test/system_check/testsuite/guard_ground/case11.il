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
        newatom      [3, 1, 'n'_2]
        newatom      [4, 0, 'ok'_0]
        newlink        [2, 0, 3, 1, 1]
        newlink        [2, 1, 3, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        findatom     [1, 0, 'ok'_0]
        anymem       [2, 0, 0, null]
        natoms         [2, 2]
        nmems          [2, 0]
        stable         [2]
        findatom     [3, 2, 'b'_2]
        deref        [5, 3, 1, 0]
        deref        [4, 3, 0, 1]
        func           [4, 'n'_2]
        eqatom         [5, 4]
        commit         ["_garb", 0]
        removeatom     [1, 0]
        removeatom     [3, 2]
        removeatom     [4, 2]
        removemem      [2, 0]
        newatom      [6, 0, 'ng'_0]
        freemem        [2]
        freeatom       [1]
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


