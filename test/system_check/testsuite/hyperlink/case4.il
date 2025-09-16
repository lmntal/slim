Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 20]
        newhlink     [1]
        getfunc      [2, 1]
        allocatomindirect[3, 2]
        newhlink     [4]
        getfunc      [5, 4]
        allocatomindirect[6, 5]
        commit         ["_init", 0]
        loadruleset    [0, @601]
        copyatom     [7, 0, 6]
        copyatom     [8, 0, 6]
        copyatom     [9, 0, 3]
        copyatom     [10, 0, 3]
        copyatom     [11, 0, 3]
        newatom      [12, 0, 'a'_1]
        newatom      [13, 0, 'b'_1]
        newatom      [14, 0, 'c'_1]
        newatom      [15, 0, 'n'_1]
        newatom      [16, 0, 3_1]
        newatom      [17, 0, 'b'_1]
        newatom      [18, 0, 'c'_1]
        newatom      [19, 0, 'ok'_0]
        newlink        [12, 0, 9, 0, 0]
        newlink        [13, 0, 10, 0, 0]
        newlink        [14, 0, 11, 0, 0]
        newlink        [15, 0, 16, 0, 0]
        newlink        [17, 0, 7, 0, 0]
        newlink        [18, 0, 8, 0, 0]
        freeatom       [6]
        freeatom       [3]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 6]
        findatom     [1, 0, 'a'_1]
        derefatom    [2, 1, 0]
        ishlink        [2]
        getnum       [3, 2]
        commit         ["_aXnu", 0]
        removeatom     [1, 0]
        removeatom     [2, 0]
        copyatom     [4, 0, 3]
        newatom      [5, 0, 'n'_1]
        newlink        [5, 0, 4, 0, 0]
        freeatom       [1]
        freeatom       [2]
        freeatom       [3]
        proceed        []


