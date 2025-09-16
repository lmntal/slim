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
        newatom      [4, 1, 'a'_1]
        newatom      [5, 1, 'm'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        anymem       [1, 0, 0, null]
        natoms         [1, 4]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'a'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'm'_1]
        findatom     [4, 1, 'b'_1]
        deref        [5, 4, 0, 0]
        func           [5, 'm'_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removeatom     [5, 1]
        removemem      [1, 0]
        newatom      [6, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        freeatom       [5]
        proceed        []


Compiled Ruleset @601 
Compiled Uniq Rule 
    --atommatch:
    --memmatch:
        spec           [1, 14]
        findatom     [1, 0, 'a'_1]
        getlink      [2, 1, 0]
        newlist      [3]
        addtolist      [3, 2]
        newlist      [4]
        isground     [5, 3, 4, []]
        uniq           [[3]]
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


