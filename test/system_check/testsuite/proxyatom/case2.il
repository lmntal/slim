Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, 'c'_1]
        newatom      [4, 2, $in_2]
        newatom      [5, 1, 'b'_1]
        newatom      [6, 1, $out_2]
        newlink        [3, 0, 4, 1, 2]
        newlink        [5, 0, 6, 1, 1]
        newlink        [6, 0, 4, 0, 1]
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
        findatom     [2, 1, 'ok'_0]
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
        spec           [1, 7]
        findatom     [1, 0, 'b'_1]
        deref        [2, 1, 0, 1]
        func           [2, $out_2]
        deref        [3, 2, 0, 0]
        func           [3, $in_2]
        deref        [5, 3, 1, 0]
        func           [5, 'c'_1]
        lockmem      [4, 3, null]
        natoms         [4, 1]
        nmems          [4, 0]
        norules        [4]
        commit         ["_bXcX", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [5, 4]
        removeatom     [3, 4]
        removemem      [4, 0]
        newatom      [6, 0, 'ok'_0]
        freemem        [4]
        freeatom       [1]
        freeatom       [2]
        freeatom       [5]
        freeatom       [3]
        proceed        []


