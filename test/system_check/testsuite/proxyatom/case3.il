Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'a'_0]
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
        findatom     [2, 1, 'd'_1]
        deref        [3, 2, 0, 1]
        func           [3, $out_2]
        deref        [4, 3, 0, 0]
        func           [4, $in_2]
        deref        [6, 4, 1, 0]
        func           [6, 'e'_1]
        lockmem      [5, 4, null]
        natoms         [5, 1]
        nmems          [5, 0]
        norules        [5]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removeatom     [6, 5]
        removeatom     [4, 5]
        removemem      [5, 1]
        removemem      [1, 0]
        newatom      [7, 0, 'ok'_0]
        freemem        [5]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [6]
        freeatom       [4]
        proceed        []


Compiled Ruleset @601  
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        findatom     [1, 0, 'a'_0]
        commit         ["_abXc", 0]
        removeatom     [1, 0]
        newmem       [2, 0, 0]
        newatom      [3, 2, 'c'_1]
        newatom      [4, 2, $in_2]
        newatom      [5, 0, 'b'_1]
        newatom      [6, 0, $out_2]
        newlink        [3, 0, 4, 1, 2]
        newlink        [5, 0, 6, 1, 0]
        newlink        [6, 0, 4, 0, 0]
        freeatom       [1]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 11]
        findatom     [1, 0, 'b'_1]
        deref        [2, 1, 0, 1]
        func           [2, $out_2]
        deref        [3, 2, 0, 0]
        func           [3, $in_2]
        deref        [5, 3, 1, 0]
        func           [5, 'c'_1]
        lockmem      [4, 3, null]
        natoms         [4, 1]
        nmems          [4, 0]
        norules        [4]
        commit         ["_bXcX", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [5, 4]
        removeatom     [3, 4]
        enqueuemem     [4]
        newatom      [7, 4, 'e'_1]
        newatom      [8, 4, $in_2]
        newatom      [9, 0, 'd'_1]
        newatom      [10, 0, $out_2]
        newlink        [7, 0, 8, 1, 4]
        newlink        [9, 0, 10, 1, 0]
        newlink        [10, 0, 8, 0, 0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [5]
        freeatom       [3]
        proceed        []


