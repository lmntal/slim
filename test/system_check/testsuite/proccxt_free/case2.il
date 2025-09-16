Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        newmem       [3, 2, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [4, 3, 4_1]
        newatom      [5, 3, $in_2]
        newatom      [6, 2, 'a'_1]
        newatom      [7, 2, $out_2]
        newlink        [4, 0, 5, 1, 3]
        newlink        [6, 0, 7, 1, 2]
        newlink        [7, 0, 5, 0, 2]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 8]
        anymem       [1, 0, 0, null]
        natoms         [1, 1]
        nmems          [1, 1]
        stable         [1]
        findatom     [2, 1, 'ok'_1]
        deref        [3, 2, 0, 1]
        func           [3, $out_2]
        deref        [4, 3, 0, 0]
        func           [4, $in_2]
        deref        [6, 4, 1, 0]
        func           [6, 4_1]
        lockmem      [5, 4, null]
        natoms         [5, 1]
        nmems          [5, 0]
        norules        [5]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [4, 5]
        removeatom     [6, 5]
        removemem      [5, 1]
        removemem      [1, 0]
        newatom      [7, 0, 'ok'_0]
        freemem        [5]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        freeatom       [6]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        anymem       [1, 0, 0, null]
        nfreelinks     [1, 0]
        norules        [1]
        findatom     [2, 1, 'a'_1]
        commit         ["_pXaX", 0]
        removeatom     [2, 1]
        removemem      [1, 0]
        removeproxies  [1]
        movecells      [0, 1]
        removetemporaryproxies[0]
        newatom      [3, 0, 'ok'_1]
        relink         [3, 0, 2, 0, 0]
        freemem        [1]
        freeatom       [2]
        proceed        []


