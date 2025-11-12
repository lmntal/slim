Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        commit         ["_init", 0]
        loadruleset    [0, @601]
        newatom      [1, 0, 'a'_0]
        newatom      [2, 0, 'a'_0]
        newatom      [3, 0, 'a'_0]
        proceed        []


Compiled Ruleset @601  
Compiled Rule 
    --priority: 10
    --atommatch:
    --memmatch:
        spec           [1, 3]
        findatom     [1, 0, 'a'_0]
        commit         ["a2b", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'b'_0]
        freeatom       [1]
        proceed        []

Compiled Rule 
    --priority: 5
    --atommatch:
    --memmatch:
        spec           [1, 3]
        findatom     [1, 0, 'b'_0]
        commit         ["b2c", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 'c'_0]
        freeatom       [1]
        proceed        []


