Compiled Ruleset @604 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        commit         ["_init", 0]
        newmem       [1, 0, 0]
        newmem       [2, 1, 0]
        loadruleset    [2, @601]
        loadruleset    [1, @602]
        loadruleset    [0, @603]
        newatom      [3, 2, 'pi'_1]
        newatom      [4, 2, 'res'_1]
        newlink        [3, 0, 4, 0, 2]
        proceed        []


Compiled Ruleset @603 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 5]
        anymem       [1, 0, 0, null]
        natoms         [1, 2]
        nmems          [1, 0]
        stable         [1]
        findatom     [2, 1, 'result'_1]
        deref        [3, 2, 0, 0]
        func           [3, 3.14_1]
        commit         ["_garb", 0]
        removeatom     [2, 1]
        removeatom     [3, 1]
        removemem      [1, 0]
        newatom      [4, 0, 'ok'_0]
        freemem        [1]
        freeatom       [2]
        freeatom       [3]
        proceed        []


Compiled Ruleset @602 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 4]
        anymem       [1, 0, 0, null]
        nfreelinks     [1, 0]
        findatom     [2, 1, 'res'_1]
        commit         ["_resp", 0]
        removeatom     [2, 1]
        removemem      [1, 0]
        removeproxies  [1]
        movecells      [0, 1]
        removetemporaryproxies[0]
        newatom      [3, 0, 'result'_1]
        relink         [3, 0, 2, 0, 0]
        freemem        [1]
        freeatom       [2]
        proceed        []


Compiled Ruleset @601 
Compiled Rule 
    --atommatch:
    --memmatch:
        spec           [1, 3]
        findatom     [1, 0, 'pi'_1]
        commit         ["_piXX", 0]
        removeatom     [1, 0]
        newatom      [2, 0, 3.14_1]
        relink         [2, 0, 1, 0, 0]
        freeatom       [1]
        proceed        []


