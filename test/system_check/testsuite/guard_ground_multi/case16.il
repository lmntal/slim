Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, 'a'_1]
        newatom      [4, 2, 'b'_2]
        newatom      [5, 2, 0_1]
        newlink        [3, 0, 4, 1, 2]
        newlink        [4, 0, 5, 0, 2]
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
        findatom     [2, 1, 'b'_0]
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
        spec           [1, 12]
        anymem       [1, 0, 0, null]
        findatom     [2, 1, 'a'_1]
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
        commit         ["_apgr", 0]
        newlist      [9]
        getlink      [10, 2, 0]
        addtolist      [9, 10]
        removeatom     [2, 1]
        removeground   [9, 1]
        removemem      [1, 0]
        newatom      [11, 0, 'b'_0]
        freemem        [1]
        freeatom       [2]
        freeground     [9]
        proceed        []


