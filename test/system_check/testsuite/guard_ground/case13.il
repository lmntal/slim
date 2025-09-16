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
        newatom      [3, 1, 'b'_1]
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
        findatom     [2, 1, 'a'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'b'_1]
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
        spec           [1, 18]
        findatom     [1, 0, 'a'_1]
        getlink      [3, 1, 0]
        findatom     [2, 0, 'b'_1]
        getlink      [4, 2, 0]
        newlist      [5]
        addtolist      [5, 3]
        newlist      [6]
        addtolist      [6, 4]
        isground     [7, 5, 6, []]
        commit         ["_axbY", 0]
        newlist      [8]
        getlink      [9, 1, 0]
        addtolist      [8, 9]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeground   [8, 0]
        copyground   [10, 8, 0, []]
        getfromlist  [11, 10, 0]
        getfromlist  [12, 10, 1]
        newatom      [13, 0, 'c'_2]
        alloclink    [14, 13, 1]
        alloclink    [15, 13, 0]
        getfromlist  [16, 11, 0]
        unifylinks     [15, 16, 0]
        getlink      [17, 2, 0]
        unifylinks     [14, 17, 0]
        freeatom       [1]
        freeatom       [2]
        freeground     [8]
        proceed        []


