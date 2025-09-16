Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 10]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        loadruleset    [1, @601]
        loadruleset    [0, @602]
        newatom      [2, 1, 'btp'_1]
        newatom      [3, 1, 'node'_3]
        newatom      [4, 1, 'node'_3]
        newatom      [5, 1, 'nil'_1]
        newatom      [6, 1, 'nil'_1]
        newatom      [7, 1, 'node'_3]
        newatom      [8, 1, 'nil'_1]
        newatom      [9, 1, 'nil'_1]
        newlink        [2, 0, 3, 2, 1]
        newlink        [3, 0, 4, 2, 1]
        newlink        [3, 1, 7, 2, 1]
        newlink        [4, 0, 5, 0, 1]
        newlink        [4, 1, 6, 0, 1]
        newlink        [7, 0, 8, 0, 1]
        newlink        [7, 1, 9, 0, 1]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        anymem       [1, 0, 0, null]
        natoms         [1, 1]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'yes'_0]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removemem      [1, 0]
        newatom      [3, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        proceed        []


Compiled Ruleset @601    
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 7]
        findatom     [1, 0, 'btp'_1]
        getlink      [2, 1, 0]
        subrule      [3, 0, "binarytree", [0, 2]]
        commit         ["isBinaryTree", 0]
        newlist      [4]
        getlink      [5, 1, 0]
        addtolist      [4, 5]
        removeatom     [1, 0]
        removeground   [4, 0]
        newatom      [6, 0, 'yes'_0]
        freeatom       [1]
        freeground     [4]
        proceed        []

Compiled Subrule @binarytree
        spec           [2, 3]
        branch         [[
                  subrule      [2, 0, "binarytree_0", [0, 1]]
                  succreturn     [2] ]]
        branch         [[
                  subrule      [2, 0, "binarytree_1", [0, 1]]
                  succreturn     [2] ]]
        failreturn     []

Compiled Subrule @binarytree_0
        spec           [2, 4]
        dereflink    [2, 1, 0]
        func           [2, 'nil'_1]
        allocset     [3]
        succreturn     [3]

Compiled Subrule @binarytree_1
        spec           [2, 8]
        dereflink    [2, 1, 2]
        func           [2, 'node'_3]
        getlink      [4, 2, 1]
        getlink      [3, 2, 0]
        subrule      [5, 0, "binarytree", [0, 3]]
        subrule      [6, 0, "binarytree", [0, 4]]
        allocset     [7]
        succreturn     [7]


