Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, 'p'_1]
        newatom      [4, 2, 'f'_1]
        newatom      [5, 2, 'p'_1]
        newatom      [6, 2, 'f'_1]
        newatom      [7, 2, 'p'_1]
        newatom      [8, 2, 'f'_1]
        newlink        [3, 0, 4, 0, 2]
        newlink        [5, 0, 6, 0, 2]
        newlink        [7, 0, 8, 0, 2]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        anymem       [1, 0, 0, null]
        natoms         [1, 0]
        nmems          [1, 1]
        stable         [1]
        anymem       [2, 1, 0, null]
        natoms         [2, 4]
        nmems          [2, 0]
        norules        [2]
        findatom     [3, 2, 'p'_1]
        deref        [4, 3, 0, 0]
        func           [4, 'f'_1]
        findatom     [5, 2, 'q'_1]
        deref        [6, 5, 0, 0]
        func           [6, 'f'_1]
        commit         ["_garb", 0]
        removeatom     [3, 2]
        removeatom     [4, 2]
        removeatom     [5, 2]
        removeatom     [6, 2]
        removemem      [2, 1]
        removemem      [1, 0]
        newatom      [7, 0, 'ok'_0]
        freemem        [2]
        freemem        [1]
        freeatom       [3]
        freeatom       [4]
        freeatom       [5]
        freeatom       [6]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        anymem       [1, 0, 0, null]
        nfreelinks     [1, 0]
        norules        [1]
        findatom     [2, 1, 'p'_1]
        findatom     [3, 1, 'f'_1]
        deref        [4, 3, 0, 0]
        neqatom        [4, 2]
        func           [4, 'p'_1]
        commit         ["_pAfX", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 1]
        removeproxies  [1]
        enqueuemem     [1]
        insertproxies  [0, 1]
        newatom      [6, 1, 'q'_1]
        relink         [6, 0, 2, 0, 1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


