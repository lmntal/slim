Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 16]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        newmem       [3, 1, 0]
        newmem       [4, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [5, 2, 'a'_2]
        newatom      [6, 2, $in_2]
        newatom      [7, 2, $in_2]
        newatom      [8, 3, 'b'_1]
        newatom      [9, 3, $in_2]
        newatom      [10, 4, 'c'_1]
        newatom      [11, 4, $in_2]
        newatom      [12, 1, $out_2]
        newatom      [13, 1, $out_2]
        newatom      [14, 1, $out_2]
        newatom      [15, 1, $out_2]
        newlink        [5, 0, 6, 1, 2]
        newlink        [5, 1, 7, 1, 2]
        newlink        [8, 0, 9, 1, 3]
        newlink        [10, 0, 11, 1, 4]
        newlink        [12, 0, 6, 0, 1]
        newlink        [12, 1, 15, 1, 1]
        newlink        [13, 0, 7, 0, 1]
        newlink        [13, 1, 14, 1, 1]
        newlink        [14, 0, 9, 0, 1]
        newlink        [15, 0, 11, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        anymem       [1, 0, 0, null]
        natoms         [1, 0]
        nmems          [1, 1]
        stable         [1]
        anymem       [2, 1, 0, null]
        natoms         [2, 3]
        nmems          [2, 0]
        norules        [2]
        findatom     [3, 2, 'b'_1]
        deref        [4, 3, 0, 1]
        func           [4, 'a'_2]
        deref        [5, 4, 0, 0]
        func           [5, 'c'_1]
        commit         ["_garb", 0]
        removeatom     [3, 2]
        removeatom     [4, 2]
        removeatom     [5, 2]
        removemem      [2, 1]
        removemem      [1, 0]
        newatom      [6, 0, 'ok'_0]
        freemem        [2]
        freemem        [1]
        freeatom       [3]
        freeatom       [4]
        freeatom       [5]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        anymem       [1, 0, 0, null]
        anymem       [2, 0, 0, null]
        neqmem         [2, 1]
        norules        [2]
        norules        [1]
        commit         ["_pqpq", 0]
        removeproxies  [1]
        removemem      [2, 0]
        removeproxies  [2]
        enqueuemem     [1]
        movecells      [1, 2]
        insertproxies  [0, 1]
        freemem        [2]
        proceed        []


