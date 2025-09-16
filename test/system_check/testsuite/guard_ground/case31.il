Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, 'w'_1]
        newatom      [4, 2, 'a'_1]
        newlink        [3, 0, 4, 0, 2]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        anymem       [1, 0, 0, null]
        natoms         [1, 0]
        nmems          [1, 1]
        stable         [1]
        anymem       [2, 1, 0, null]
        natoms         [2, 2]
        nmems          [2, 0]
        norules        [2]
        findatom     [3, 2, 'x'_1]
        deref        [4, 3, 0, 0]
        func           [4, 'a'_1]
        commit         ["_garb", 0]
        removeatom     [3, 2]
        removeatom     [4, 2]
        removemem      [2, 1]
        removemem      [1, 0]
        newatom      [5, 0, 'ok'_0]
        freemem        [2]
        freemem        [1]
        freeatom       [3]
        freeatom       [4]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 18]
        anymem       [1, 0, 0, null]
        findatom     [2, 1, 'w'_1]
        getlink      [3, 2, 0]
        newlist      [4]
        addtolist      [4, 3]
        newlist      [5]
        isground     [6, 4, 5, []]
        loadfunc     [7, 1_1]
        iaddfunc     [8, 7, 6]
        natomsindirect [1, 8]
        nmems          [1, 0]
        norules        [1]
        commit         ["_wDgr", 0]
        newlist      [9]
        getlink      [10, 2, 0]
        addtolist      [9, 10]
        removeatom     [2, 1]
        removeground   [9, 1]
        enqueuemem     [1]
        copyground   [12, 9, 1, []]
        getfromlist  [13, 12, 0]
        getfromlist  [14, 12, 1]
        newatom      [15, 1, 'x'_1]
        alloclink    [16, 15, 0]
        getfromlist  [17, 13, 0]
        unifylinks     [16, 17, 1]
        freeatom       [2]
        freeground     [9]
        proceed        []


