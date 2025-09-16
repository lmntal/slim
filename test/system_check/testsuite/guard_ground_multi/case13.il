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
        newatom      [3, 2, 'a'_4]
        newatom      [4, 2, 't'_2]
        newatom      [5, 2, 't'_2]
        newlink        [3, 0, 4, 0, 2]
        newlink        [3, 1, 4, 1, 2]
        newlink        [3, 2, 5, 0, 2]
        newlink        [3, 3, 5, 1, 2]
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
        spec           [1, 23]
        anymem       [1, 0, 0, null]
        findatom     [2, 1, 'a'_4]
        getlink      [6, 2, 3]
        getlink      [5, 2, 2]
        getlink      [4, 2, 1]
        getlink      [3, 2, 0]
        newlist      [7]
        addtolist      [7, 3]
        addtolist      [7, 4]
        newlist      [8]
        addtolist      [8, 5]
        addtolist      [8, 6]
        isground     [9, 7, 8, []]
        newlist      [10]
        addtolist      [10, 5]
        addtolist      [10, 6]
        newlist      [11]
        addtolist      [11, 3]
        addtolist      [11, 4]
        isground     [12, 10, 11, []]
        loadfunc     [13, 1_1]
        iaddfunc     [14, 13, 9]
        iaddfunc     [15, 14, 12]
        natomsindirect [1, 15]
        nmems          [1, 0]
        norules        [1]
        commit         ["_aXYZ", 0]
        newlist      [16]
        getlink      [17, 2, 0]
        addtolist      [16, 17]
        getlink      [18, 2, 1]
        addtolist      [16, 18]
        newlist      [19]
        getlink      [20, 2, 2]
        addtolist      [19, 20]
        getlink      [21, 2, 3]
        addtolist      [19, 21]
        removeatom     [2, 1]
        removeground   [16, 1]
        removeground   [19, 1]
        removemem      [1, 0]
        newatom      [22, 0, 'b'_0]
        freemem        [1]
        freeatom       [2]
        freeground     [16]
        freeground     [19]
        proceed        []


