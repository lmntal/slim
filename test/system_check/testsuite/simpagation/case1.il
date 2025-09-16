Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_0]
        newatom      [3, 1, 'b'_0]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        anymem       [1, 0, 0, null]
        natoms         [1, 2]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'a'_0]
        findatom     [3, 1, 'c'_0]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removemem      [1, 0]
        newatom      [4, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        findatom     [1, 0, 'a'_0]
        findatom     [2, 0, 'b'_0]
        commit         ["_abca", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        newatom      [3, 0, 'c'_0]
        newatom      [4, 0, 'a'_0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


