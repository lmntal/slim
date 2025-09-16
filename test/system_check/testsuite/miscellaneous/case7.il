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
        newatom      [3, 2, 'f'_2]
        newatom      [4, 2, 'g'_2]
        newlink        [3, 0, 4, 0, 2]
        newlink        [3, 1, 4, 1, 2]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        anymem       [1, 0, 0, null]
        natoms         [1, 2]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'g'_2]
        deref        [3, 2, 0, 1]
        eqatom         [3, 2]
        findatom     [4, 1, 'g'_2]
        neqatom        [4, 2]
        deref        [5, 4, 0, 1]
        eqatom         [5, 4]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [4, 1]
        removemem      [1, 0]
        newatom      [6, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [4]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 16]
        anymem       [1, 0, 0, null]
        nfreelinks     [1, 0]
        norules        [1]
        findatom     [2, 1, 'f'_2]
        commit         ["_pXYf", 0]
        removeatom     [2, 1]
        removemem      [1, 0]
        removeproxies  [1]
        getlink      [3, 2, 0]
        getlink      [4, 2, 1]
        insertconnectors[5, [3, 4], 1]
        copycells    [6, 0, 1]
        copycells    [7, 0, 1]
        removetemporaryproxies[0]
        getlink      [8, 2, 0]
        getlink      [9, 2, 1]
        lookuplink   [10, 6, 8]
        lookuplink   [11, 6, 9]
        unifylinks     [10, 11, 0]
        getlink      [12, 2, 0]
        getlink      [13, 2, 1]
        lookuplink   [14, 7, 12]
        lookuplink   [15, 7, 13]
        unifylinks     [14, 15, 0]
        deleteconnectors[5, 6]
        deleteconnectors[5, 7]
        dropmem        [1]
        freemem        [1]
        freeatom       [2]
        proceed        []


