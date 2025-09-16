Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        setmemname     [2, "m"]
        loadruleset    [2, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, 'a'_1]
        newatom      [4, 2, 'b'_1]
        newlink        [3, 0, 4, 0, 2]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        anymem       [1, 0, 0, null]
        natoms         [1, 0]
        nmems          [1, 1]
        stable         [1]
        anymem       [2, 1, 0, "m"]
        natoms         [2, 1]
        nmems          [2, 0]
        findatom     [3, 2, 'c'_0]
        commit         ["_garb", 0]
        removeatom     [3, 2]
        removemem      [2, 1]
        removemem      [1, 0]
        newatom      [4, 0, 'ok'_0]
        freemem        [2]
        freemem        [1]
        freeatom       [3]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        findatom     [1, 0, 'a'_1]
        deref        [2, 1, 0, 0]
        func           [2, 'b'_1]
        commit         ["_abc", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        newatom      [3, 0, 'c'_0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


