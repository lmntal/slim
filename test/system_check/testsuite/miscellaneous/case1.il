Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_2]
        newlink        [2, 0, 2, 1, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        anymem       [1, 0, 0, null]
        natoms         [1, 1]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'b'_2]
        deref        [3, 2, 0, 1]
        eqatom         [3, 2]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removemem      [1, 0]
        newatom      [4, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        findatom     [1, 0, 'a'_2]
        deref        [2, 1, 0, 1]
        eqatom         [2, 1]
        commit         ["_aXXb", 0]
        removeatom     [1, 0]
        newatom      [3, 0, 'b'_2]
        newlink        [3, 0, 3, 1, 0]
        freeatom       [1]
        proceed        []


