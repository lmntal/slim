Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, '+'_1]
        newatom      [4, 2, $in_2]
        newatom      [5, 1, 'a'_1]
        newatom      [6, 1, $out_2]
        newatom      [7, 0, 'ok'_0]
        newlink        [3, 0, 4, 1, 2]
        newlink        [5, 0, 6, 1, 1]
        newlink        [6, 0, 4, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        findatom     [1, 0, 'ok'_0]
        anymem       [2, 0, 0, null]
        natoms         [2, 1]
        nmems          [2, 1]
        stable         [2]
        findatom     [3, 2, 'b'_1]
        deref        [4, 3, 0, 1]
        func           [4, $out_2]
        deref        [5, 4, 0, 0]
        func           [5, $in_2]
        deref        [7, 5, 1, 0]
        func           [7, '+'_1]
        lockmem      [6, 5, null]
        natoms         [6, 1]
        nmems          [6, 0]
        norules        [6]
        commit         ["_garb", 0]
        removeatom     [1, 0]
        removeatom     [3, 2]
        removeatom     [4, 2]
        removeatom     [7, 6]
        removeatom     [5, 6]
        removemem      [6, 2]
        removemem      [2, 0]
        newatom      [8, 0, 'ng'_0]
        freemem        [6]
        freemem        [2]
        freeatom       [1]
        freeatom       [3]
        freeatom       [4]
        freeatom       [7]
        freeatom       [5]
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


