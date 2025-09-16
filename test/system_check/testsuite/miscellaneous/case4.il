Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 10]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 2, 'f'_2]
        newatom      [4, 2, 'f'_2]
        newatom      [5, 2, 'f'_2]
        newatom      [6, 2, 'f'_2]
        newatom      [7, 1, 'c'_0]
        newatom      [8, 1, 'c'_0]
        newatom      [9, 1, 'c'_0]
        newlink        [3, 0, 4, 1, 2]
        newlink        [3, 1, 6, 0, 2]
        newlink        [4, 0, 5, 1, 2]
        newlink        [5, 0, 6, 1, 2]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 9]
        anymem       [1, 0, 0, null]
        natoms         [1, 0]
        nmems          [1, 1]
        stable         [1]
        anymem       [2, 1, 0, null]
        natoms         [2, 4]
        nmems          [2, 0]
        norules        [2]
        findatom     [3, 2, 'f'_2]
        deref        [5, 3, 1, 0]
        func           [5, 'g'_2]
        deref        [7, 5, 1, 0]
        deref        [4, 3, 0, 1]
        func           [4, 'g'_2]
        deref        [6, 4, 0, 1]
        func           [6, 'g'_2]
        eqatom         [7, 6]
        commit         ["_garb", 0]
        removeatom     [3, 2]
        removeatom     [4, 2]
        removeatom     [6, 2]
        removeatom     [5, 2]
        removemem      [2, 1]
        removemem      [1, 0]
        newatom      [8, 0, 'ok'_0]
        freemem        [2]
        freemem        [1]
        freeatom       [3]
        freeatom       [4]
        freeatom       [6]
        freeatom       [5]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        findatom     [1, 0, 'c'_0]
        anymem       [2, 0, 0, null]
        nfreelinks     [2, 0]
        norules        [2]
        findatom     [3, 2, 'f'_2]
        commit         ["_cXfY", 0]
        removeatom     [1, 0]
        removeatom     [3, 2]
        removeproxies  [2]
        enqueuemem     [2]
        insertproxies  [0, 2]
        newatom      [5, 2, 'g'_2]
        relink         [5, 0, 3, 0, 2]
        relink         [5, 1, 3, 1, 2]
        freeatom       [1]
        freeatom       [3]
        proceed        []


