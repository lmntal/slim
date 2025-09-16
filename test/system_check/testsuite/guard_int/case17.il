Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'neq'_2]
        newatom      [3, 1, 10_1]
        newatom      [4, 1, 10_1]
        newatom      [5, 0, 'ok'_0]
        newlink        [2, 0, 3, 0, 1]
        newlink        [2, 1, 4, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        findatom     [1, 0, 'ok'_0]
        anymem       [2, 0, 0, null]
        natoms         [2, 1]
        nmems          [2, 0]
        stable         [2]
        findatom     [3, 2, 'yes'_0]
        commit         ["_garb", 0]
        removeatom     [1, 0]
        removeatom     [3, 2]
        removemem      [2, 0]
        newatom      [4, 0, 'ng'_0]
        freemem        [2]
        freeatom       [1]
        freeatom       [3]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 15]
        findatom     [1, 0, 'neq'_2]
        getlink      [3, 1, 1]
        getlink      [2, 1, 0]
        newlist      [4]
        addtolist      [4, 2]
        newlist      [5]
        addtolist      [5, 3]
        isground     [6, 4, 5, []]
        newlist      [7]
        addtolist      [7, 3]
        neqground      [4, 7]
        newlist      [8]
        addtolist      [8, 2]
        isground     [9, 7, 8, []]
        commit         ["_neqi", 0]
        newlist      [10]
        getlink      [11, 1, 1]
        addtolist      [10, 11]
        newlist      [12]
        getlink      [13, 1, 0]
        addtolist      [12, 13]
        removeatom     [1, 0]
        removeground   [12, 0]
        removeground   [10, 0]
        newatom      [14, 0, 'yes'_0]
        freeatom       [1]
        freeground     [12]
        freeground     [10]
        proceed        []


