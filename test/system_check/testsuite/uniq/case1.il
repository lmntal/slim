Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_0]
        newatom      [3, 1, 'a'_0]
        newatom      [4, 1, 'a'_0]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        anymem       [1, 0, 0, null]
        natoms         [1, 3]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'a'_0]
        findatom     [3, 1, 'a'_0]
        neqatom        [3, 2]
        findatom     [4, 1, 'b'_0]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removemem      [1, 0]
        newatom      [5, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


Compiled Ruleset @601 
Compiled Uniq Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        findatom     [1, 0, 'a'_0]
        uniq           [[]]
        commit         ["_auni", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'b'_0]
        freeatom       [1]
        proceed        []


