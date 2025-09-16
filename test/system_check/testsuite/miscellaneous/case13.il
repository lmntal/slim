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
        newatom      [3, 2, 'f'_1]
        newatom      [4, 2, 'f'_1]
        newatom      [5, 2, 'f'_1]
        newatom      [6, 2, 'f'_1]
        newlink        [3, 0, 4, 0, 2]
        newlink        [5, 0, 6, 0, 2]
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
        findatom     [2, 1, 'f'_1]
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
        spec           [1, 6]
        anymem       [1, 0, 0, null]
        nfreelinks     [1, 0]
        norules        [1]
        findatom     [2, 1, 'f'_1]
        findatom     [3, 1, 'f'_1]
        neqatom        [3, 2]
        findatom     [4, 1, 'f'_1]
        neqatom        [4, 2]
        neqatom        [4, 3]
        commit         ["_pXYZ", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removemem      [1, 0]
        removeproxies  [1]
        movecells      [0, 1]
        removetemporaryproxies[0]
        newatom      [5, 0, 'g'_1]
        relink         [5, 0, 4, 0, 0]
        unify          [2, 0, 3, 0, 0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


