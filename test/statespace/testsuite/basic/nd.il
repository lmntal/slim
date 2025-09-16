Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 2]
        commit         ["_init", 0]
        loadruleset    [0, @601]
        newatom      [1, 0, 'a'_0]
        proceed        []


Compiled Ruleset @601      
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        findatom     [1, 0, 'a'_0]
        commit         ["_ab", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'b'_0]
        freeatom       [1]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        findatom     [1, 0, 'a'_0]
        commit         ["_ac", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'c'_0]
        freeatom       [1]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        findatom     [1, 0, 'a'_0]
        commit         ["_abc", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'b'_0]
        newatom      [3, 0, 'c'_0]
        freeatom       [1]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        findatom     [1, 0, 'b'_0]
        commit         ["_bc", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'c'_0]
        freeatom       [1]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        findatom     [1, 0, 'c'_0]
        commit         ["_cdd", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'd'_0]
        newatom      [3, 0, 'd'_0]
        freeatom       [1]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        findatom     [1, 0, 'd'_0]
        findatom     [2, 0, 'd'_0]
        neqatom        [2, 1]
        findatom     [3, 0, 'd'_0]
        neqatom        [3, 1]
        neqatom        [3, 2]
        findatom     [4, 0, 'd'_0]
        neqatom        [4, 1]
        neqatom        [4, 2]
        neqatom        [4, 3]
        commit         ["_dddd", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        removeatom     [3, 0]
        removeatom     [4, 0]
        newatom      [5, 0, 'd'_0]
        newatom      [6, 0, 'd'_0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [3]
        freeatom       [4]
        proceed        []


