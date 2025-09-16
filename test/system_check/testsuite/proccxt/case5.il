Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 13]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        newmem       [3, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [4, 2, 'a'_2]
        newatom      [5, 2, $in_2]
        newatom      [6, 2, $in_2]
        newatom      [7, 3, 'b'_1]
        newatom      [8, 3, $in_2]
        newatom      [9, 1, 'c'_1]
        newatom      [10, 1, $out_2]
        newatom      [11, 1, $out_2]
        newatom      [12, 1, $out_2]
        newlink        [4, 0, 5, 1, 2]
        newlink        [4, 1, 6, 1, 2]
        newlink        [7, 0, 8, 1, 3]
        newlink        [9, 0, 10, 1, 1]
        newlink        [10, 0, 5, 0, 1]
        newlink        [11, 0, 6, 0, 1]
        newlink        [11, 1, 12, 1, 1]
        newlink        [12, 0, 8, 0, 1]
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
        findatom     [2, 1, 'b'_1]
        deref        [3, 2, 0, 1]
        func           [3, 'a'_2]
        deref        [4, 3, 0, 0]
        func           [4, 'c'_1]
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
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        anymem       [1, 0, 0, null]
        anymem       [2, 0, 0, null]
        neqmem         [2, 1]
        norules        [2]
        norules        [1]
        commit         ["_pqpq", 0]
        removemem      [1, 0]
        removeproxies  [1]
        removemem      [2, 0]
        removeproxies  [2]
        movecells      [0, 1]
        movecells      [0, 2]
        removetemporaryproxies[0]
        freemem        [1]
        freemem        [2]
        proceed        []


