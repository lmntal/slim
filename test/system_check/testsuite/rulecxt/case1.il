Compiled Ruleset @604 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [2, @601]
        loadruleset    [1, @602]
        loadruleset    [0, @603]
        newatom      [3, 1, 'a'_0]
        proceed        []


Compiled Ruleset @603 
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


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 2]
        anymem       [1, 0, 0, null]
        natoms         [1, 0]
        nmems          [1, 0]
        commit         ["_pp", 0]
        removemem      [1, 0]
        copyrules      [0, 1]
        freemem        [1]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        findatom     [1, 0, 'a'_0]
        commit         ["_ab", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'b'_0]
        freeatom       [1]
        proceed        []


