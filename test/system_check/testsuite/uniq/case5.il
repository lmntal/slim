Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_1]
        newatom      [3, 1, 1_1]
        newatom      [4, 1, 'a'_1]
        newatom      [5, 1, 2_1]
        newatom      [6, 1, 'a'_1]
        newatom      [7, 1, 2_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
        newlink        [6, 0, 7, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        anymem       [1, 0, 0, null]
        natoms         [1, 6]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'b'_1]
        deref        [3, 2, 0, 0]
        func           [3, 1_1]
        findatom     [4, 1, 'b'_1]
        neqatom        [4, 2]
        deref        [5, 4, 0, 0]
        func           [5, 2_1]
        findatom     [6, 1, 'a'_1]
        deref        [7, 6, 0, 0]
        func           [7, 2_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [4, 1]
        removeatom     [6, 1]
        removeatom     [3, 1]
        removeatom     [5, 1]
        removeatom     [7, 1]
        removemem      [1, 0]
        newatom      [8, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [4]
        freeatom       [6]
        freeatom       [3]
        freeatom       [5]
        freeatom       [7]
        proceed        []


Compiled Ruleset @601 
Compiled Uniq Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        findatom     [1, 0, 'a'_1]
        derefatom    [6, 1, 0]
        isunary        [6]
        getlink      [2, 1, 0]
        newlist      [3]
        addtolist      [3, 2]
        newlist      [4]
        isground     [5, 3, 4, []]
        uniq           [[3]]
        commit         ["_axun", 0]
        removeatom     [1, 0]
        removeatom     [6, 0]
        copyatom     [7, 0, 6]
        newatom      [8, 0, 'b'_1]
        newlink        [8, 0, 7, 0, 0]
        freeatom       [1]
        freeatom       [6]
        proceed        []


