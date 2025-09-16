Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [3, 1, '1'_0]
        newatom      [4, 1, '1'_0]
        newatom      [5, 1, '1'_0]
        newatom      [6, 1, '1'_0]
        newatom      [7, 1, '1'_0]
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
        natoms         [2, 5]
        nmems          [2, 0]
        norules        [2]
        findatom     [3, 2, '1'_0]
        findatom     [4, 2, '1'_0]
        neqatom        [4, 3]
        findatom     [5, 2, '1'_0]
        neqatom        [5, 3]
        neqatom        [5, 4]
        findatom     [6, 2, '1'_0]
        neqatom        [6, 3]
        neqatom        [6, 4]
        neqatom        [6, 5]
        findatom     [7, 2, '1'_0]
        neqatom        [7, 3]
        neqatom        [7, 4]
        neqatom        [7, 5]
        neqatom        [7, 6]
        commit         ["_garb", 0]
        removeatom     [3, 2]
        removeatom     [4, 2]
        removeatom     [5, 2]
        removeatom     [6, 2]
        removeatom     [7, 2]
        removemem      [2, 1]
        removemem      [1, 0]
        newatom      [8, 0, 'ok'_0]
        freemem        [2]
        freemem        [1]
        freeatom       [3]
        freeatom       [4]
        freeatom       [5]
        freeatom       [6]
        freeatom       [7]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        findatom     [1, 0, '1'_0]
        anymem       [2, 0, 0, null]
        norules        [2]
        commit         ["_p1p1", 0]
        removeatom     [1, 0]
        removeproxies  [2]
        enqueuemem     [2]
        insertproxies  [0, 2]
        newatom      [4, 2, '1'_0]
        freeatom       [1]
        proceed        []


