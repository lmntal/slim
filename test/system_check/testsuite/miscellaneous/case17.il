Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, 'e'_1]
        newatom      [4, 2, 'f'_5]
        newatom      [5, 2, 'h'_4]
        newlink        [3, 0, 4, 0, 2]
        newlink        [4, 1, 5, 0, 2]
        newlink        [4, 2, 5, 1, 2]
        newlink        [4, 3, 5, 2, 2]
        newlink        [4, 4, 5, 3, 2]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        anymem       [1, 0, 0, null]
        natoms         [1, 3]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'g'_1]
        deref        [3, 2, 0, 3]
        func           [3, 'h'_4]
        deref        [5, 3, 1, 2]
        eqatom         [5, 3]
        deref        [4, 3, 0, 0]
        func           [4, 'e'_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removemem      [1, 0]
        newatom      [6, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        anymem       [1, 0, 0, null]
        nfreelinks     [1, 0]
        norules        [1]
        findatom     [2, 1, 'f'_5]
        commit         ["_pABC", 0]
        removeatom     [2, 1]
        removemem      [1, 0]
        removeproxies  [1]
        movecells      [0, 1]
        removetemporaryproxies[0]
        newatom      [3, 0, 'g'_1]
        relink         [3, 0, 2, 4, 0]
        unify          [2, 0, 2, 1, 0]
        unify          [2, 2, 2, 3, 0]
        freemem        [1]
        freeatom       [2]
        proceed        []


