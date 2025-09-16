Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [0, @601]
        newatom      [3, 2, 'c'_1]
        newatom      [4, 2, $in_2]
        newatom      [5, 1, 'b'_1]
        newatom      [6, 1, $out_2]
        newlink        [3, 0, 4, 1, 2]
        newlink        [5, 0, 6, 1, 1]
        newlink        [6, 0, 4, 0, 1]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        anymem       [1, 0, 0, null]
        natoms         [1, 1]
        nmems          [1, 1]
        stable         [1]
        findatom     [2, 1, 'b'_1]
        deref        [3, 2, 0, 1]
        func           [3, $out_2]
        deref        [4, 3, 0, 0]
        func           [4, $in_2]
        deref        [6, 4, 1, 0]
        func           [6, 'c'_1]
        lockmem      [5, 4, null]
        natoms         [5, 1]
        nmems          [5, 0]
        norules        [5]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [6, 5]
        removeatom     [4, 5]
        removemem      [5, 1]
        removemem      [1, 0]
        newatom      [7, 0, 'ok'_0]
        freemem        [5]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [6]
        freeatom       [4]
        proceed        []


