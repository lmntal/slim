Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'u'_1]
        newatom      [3, 1, 'a'_1]
        newatom      [4, 1, 'v'_1]
        newatom      [5, 1, 'a'_1]
        newlink        [2, 0, 3, 0, 1]
        newlink        [4, 0, 5, 0, 1]
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
        findatom     [2, 1, 'yes'_0]
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
        spec           [1, 8]
        findatom     [1, 0, 'u'_1]
        derefatom    [3, 1, 0]
        isunary        [3]
        getfunc      [6, 3]
        findatom     [2, 0, 'v'_1]
        derefatom    [4, 2, 0]
        isunary        [4]
        getfunc      [5, 4]
        eqfunc         [5, 6]
        commit         ["_uuvv", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [3, 0]
        removeatom     [4, 0]
        newatom      [7, 0, 'yes'_0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


