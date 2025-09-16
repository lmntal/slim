Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 11]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        newmem       [3, 2, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [4, 3, 'a'_1]
        newatom      [5, 3, $in_2]
        newatom      [6, 2, 'b'_2]
        newatom      [7, 2, $out_2]
        newatom      [8, 2, $in_2]
        newatom      [9, 1, 'c'_1]
        newatom      [10, 1, $out_2]
        newlink        [4, 0, 5, 1, 3]
        newlink        [6, 0, 7, 1, 2]
        newlink        [6, 1, 8, 1, 2]
        newlink        [7, 0, 5, 0, 2]
        newlink        [9, 0, 10, 1, 1]
        newlink        [10, 0, 8, 0, 1]
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
        findatom     [2, 1, 'c'_1]
        deref        [3, 2, 0, 1]
        func           [3, 'b'_2]
        deref        [4, 3, 0, 0]
        func           [4, 'a'_1]
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
        norules        [1]
        anymem       [2, 1, 0, null]
        norules        [2]
        commit         ["_pqpq", 0]
        removemem      [2, 1]
        removeproxies  [2]
        removemem      [1, 0]
        removeproxies  [1]
        movecells      [0, 2]
        movecells      [0, 1]
        removetemporaryproxies[0]
        freemem        [2]
        freemem        [1]
        proceed        []


