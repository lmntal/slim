Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, 'e'_1]
        newatom      [4, 2, 'f'_5]
        newlink        [3, 0, 4, 0, 2]
        newlink        [4, 1, 4, 2, 2]
        newlink        [4, 3, 4, 4, 2]
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
        findatom     [2, 1, 'e'_1]
        deref        [3, 2, 0, 0]
        func           [3, 'g'_1]
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


