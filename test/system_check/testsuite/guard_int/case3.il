Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'i'_1]
        newatom      [3, 1, "3"_1]
        newatom      [4, 0, 'ok'_0]
        newlink        [2, 0, 3, 0, 1]
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
        spec           [1, 4]
        findatom     [1, 0, 'i'_1]
        derefatom    [2, 1, 0]
        isint          [2]
        commit         ["_iiin", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        newatom      [3, 0, 'yes'_0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


