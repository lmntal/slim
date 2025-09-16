Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        commit         ["_init", 0]
        loadruleset    [0, @601]
        newatom      [1, 0, 'a'_0]
        newatom      [2, 0, 'a'_0]
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
        spec           [1, 3]
        findatom     [1, 0, 'a'_0]
        commit         ["_ad", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'd'_0]
        freeatom       [1]
        proceed        []

Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        findatom     [1, 0, 'd'_0]
        findatom     [2, 0, 'd'_0]
        neqatom        [2, 1]
        commit         ["_ddaa", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        newatom      [3, 0, 'a'_0]
        newatom      [4, 0, 'a'_0]
        freeatom       [1]
        freeatom       [2]
        proceed        []


