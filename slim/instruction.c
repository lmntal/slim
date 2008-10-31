/*
 * instruction.c - Intermediate code instructions
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lang@ueda.info.waseda.ac.jp>
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are
 *   met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 *    3. Neither the name of the Ueda Laboratory LMNtal Group nor the
 *       names of its contributors may be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: instruction.c,v 1.4 2008/10/17 07:25:36 iwasawa Exp $
 */

#include "instruction.h"

struct InstrSpec spec[] = {
    {"deref", INSTR_DEREF, {InstrVar, InstrVar, InstrVar, InstrVar}},
    {"derefatom", INSTR_DEREFATOM, {InstrVar, InstrVar, InstrVar}},
    {"dereflink", INSTR_DEREFLINK, {InstrVar, InstrVar, InstrVar}},
    {"findatom", INSTR_FINDATOM, {InstrVar, InstrVar, ArgFunctor}},
    {"findatom2", INSTR_FINDATOM2, {InstrVar, InstrVar, InstrVar, ArgFunctor}},

    {"lockmem", INSTR_LOCKMEM, {InstrVar, InstrVar, String}},
    {"anymem", INSTR_ANYMEM, {InstrVar, InstrVar, InstrVar, String}},

    {"getmem", INSTR_GETMEM, {InstrVar, InstrVar, InstrVar, String}},
    {"getparent", INSTR_GETPARENT, {InstrVar, InstrVar}},

    {"testmem", INSTR_TESTMEM, {InstrVar, InstrVar}},
    {"norules", INSTR_NORULES, {InstrVar}},
    {"nfreelinks", INSTR_NFREELINKS, {InstrVar, InstrVar}},
    {"natoms", INSTR_NATOMS, {InstrVar, InstrVar}},
    {"natomsindirect", INSTR_NATOMSINDIRECT, {InstrVar, InstrVar}},
    {"nmems", INSTR_NMEMS, {InstrVar, InstrVar}},
    {"eqmem", INSTR_EQMEM, {InstrVar, InstrVar}},
    {"neqmem", INSTR_NEQMEM, {InstrVar, InstrVar}},
    {"stable", INSTR_STABLE, {InstrVar}},

    {"func", INSTR_FUNC, {InstrVar, ArgFunctor}},
    {"notfunc", INSTR_NOTFUNC, {InstrVar, ArgFunctor}},
    {"eqatom", INSTR_EQATOM, {InstrVar, InstrVar}},
    {"neqatom", INSTR_NEQATOM, {InstrVar, InstrVar}},
    {"samefunc", INSTR_SAMEFUNC, {InstrVar, InstrVar}},

    {"dereffunc", INSTR_DEREFFUNC, {InstrVar, InstrVar, InstrVar}},
    {"getfunc", INSTR_GETFUNC, {InstrVar, InstrVar}},
    {"loadfunc", INSTR_LOADFUNC, {InstrVar, ArgFunctor}},
    {"eqfunc", INSTR_EQFUNC, {InstrVar, InstrVar}},
    {"neqfunc", INSTR_NEQFUNC, {InstrVar, InstrVar}},

    {"removeatom", INSTR_REMOVEATOM, {InstrVar, InstrVar, ArgFunctor}},
    {"removeatom", INSTR_REMOVEATOM, {InstrVar, InstrVar}},
    {"newatom", INSTR_NEWATOM, {InstrVar, InstrVar, ArgFunctor}},
    {"newatomindirect", INSTR_NEWATOMINDIRECT, {InstrVar, InstrVar, InstrVar}},
    {"enqueueatom", INSTR_ENQUEUEATOM, {InstrVar}},
    {"dequeueatom", INSTR_DEQUEUEATOM, {InstrVar}},
    {"freeatom", INSTR_FREEATOM, {InstrVar}},
    {"alterfunc", INSTR_ALTERFUNC, {InstrVar, ArgFunctor}},

    {"allocatom", INSTR_ALLOCATOM, {InstrVar, ArgFunctor}},
    {"allocatomindirect", INSTR_ALLOCATOMINDIRECT, {InstrVar, InstrVar}},
    {"copyatom", INSTR_COPYATOM, {InstrVar, InstrVar, InstrVar}},
    {"addatom", INSTR_ADDATOM, {InstrVar, InstrVar}},

    {"removemem", INSTR_REMOVEMEM, {InstrVar, InstrVar}},
    {"newmem", INSTR_NEWMEM, {InstrVar, InstrVar, InstrVar}},
    {"allocmem", INSTR_ALLOCMEM, {InstrVar}},
    {"newroot", INSTR_NEWROOT, {InstrVar, InstrVar, InstrVar, InstrVar}},
    {"movecells", INSTR_MOVECELLS, {InstrVar, InstrVar}},
    {"enqueueallatoms", INSTR_ENQUEUEALLATOMS, {InstrVar}},
    {"freemem", INSTR_FREEMEM, {InstrVar}},
    {"addmem", INSTR_ADDMEM, {InstrVar, InstrVar}},
    {"enqueuemem", INSTR_ENQUEUEMEM, {InstrVar}},
    {"unlockmem", INSTR_UNLOCKMEM, {InstrVar}},
    {"setmemname", INSTR_SETMEMNAME, {InstrVar, String}},

    {"getlink", INSTR_GETLINK, {InstrVar, InstrVar, InstrVar}},
    {"alloclink", INSTR_ALLOCLINK, {InstrVar, InstrVar, InstrVar}},

    {"newlink", INSTR_NEWLINK, {InstrVar, InstrVar, InstrVar, InstrVar, InstrVar}},
    {"relink", INSTR_RELINK, {InstrVar, InstrVar, InstrVar, InstrVar, InstrVar}},
    {"unify", INSTR_UNIFY, {InstrVar, InstrVar, InstrVar, InstrVar, InstrVar}},
    {"inheritlink", INSTR_INHERITLINK, {InstrVar, InstrVar, InstrVar, InstrVar}},
    {"unifylinks", INSTR_UNIFYLINKS, {InstrVar, InstrVar, InstrVar}},

    {"removeproxies", INSTR_REMOVEPROXIES, {InstrVar}},
    {"removetoplevelproxies", INSTR_REMOVETOPLEVELPROXIES, {InstrVar}},
    {"insertproxies", INSTR_INSERTPROXIES, {InstrVar, InstrVar}},
    {"removetemporaryproxies", INSTR_REMOVETEMPORARYPROXIES, {InstrVar}},

    {"loadruleset", INSTR_LOADRULESET, {InstrVar, ArgRuleset}},
    {"copyrules", INSTR_COPYRULES, {InstrVar, InstrVar}},
    {"clearrules", INSTR_CLEARRULES, {InstrVar}},
    {"loadmodule", INSTR_LOADMODULE, {InstrVar, String}},

    {"recursivelock", INSTR_RECURSIVELOCK, {InstrVar}},
    {"recursiveunlock", INSTR_RECURSIVEUNLOCK, {InstrVar}},
    {"copycells", INSTR_COPYCELLS, {InstrVar, InstrVar, InstrVar}},
    {"dropmem", INSTR_DROPMEM, {InstrVar}},
    {"lookuplink", INSTR_LOOKUPLINK, {InstrVar, InstrVar, InstrVar}},
    {"insertconnectors", INSTR_INSERTCONNECTORS, {InstrVar, InstrVarList, InstrVar}},
    {"insertconnectorsinnull", INSTR_INSERTCONNECTORSINNULL, {InstrVar, InstrVarList}},
    {"deleteconnectors", INSTR_DELETECONNECTORS, {InstrVar, InstrVar}},

    {"react", INSTR_REACT, {InstrVar, InstrVarList, InstrVarList, InstrVarList}},
    {"jump", INSTR_JUMP, {Label, InstrVarList, InstrVarList, InstrVarList}},
    {"commit", INSTR_COMMIT, {String, LineNum}},
    {"resetvars", INSTR_RESETVARS, {InstrVarList, InstrVarList, InstrVarList,}},
    {"changevars", INSTR_CHANGEVARS, {InstrVarList, InstrVarList, InstrVarList,}},
    {"spec", INSTR_SPEC, {InstrVar, InstrVar}},
    {"proceed", INSTR_PROCEED, {}},
    {"stop", INSTR_STOP, {}},

    {"loop", INSTR_LOOP, {InstrList}},
    {"run", INSTR_RUN, {InstrList}},
    {"not", INSTR_NOT, {InstrList}},
    {"inline", INSTR_INLINE, {InstrVar, String, InstrVar}},

    /* special */
    {"group", INSTR_GROUP, {InstrList}},
    {"branch", INSTR_BRANCH, {InstrList}},

    /* guard */
    {"eqground", INSTR_EQGROUND, {InstrVar, InstrVar}},
    {"neqground", INSTR_NEQGROUND, {InstrVar, InstrVar}},
    {"copyground", INSTR_COPYGROUND, {InstrVar, InstrVar, InstrVar}},
    {"removeground", INSTR_REMOVEGROUND, {InstrVar, InstrVar}},
    {"freeground", INSTR_FREEGROUND, {InstrVar}},
    {"isground", INSTR_ISGROUND, {InstrVar, InstrVar, InstrVar}},
    {"isunary", INSTR_ISUNARY, {InstrVar}},
    {"isint", INSTR_ISINT, {InstrVar}},
    {"isintfunc", INSTR_ISINTFUNC, {InstrVar}},

    {"isfloat", INSTR_ISFLOAT, {InstrVar}},

    /* guard: float */
    {"fadd", INSTR_FADD, {InstrVar, InstrVar, InstrVar}},
    {"fsub", INSTR_FSUB, {InstrVar, InstrVar, InstrVar}},
    {"fmul", INSTR_FMUL, {InstrVar, InstrVar, InstrVar}},
    {"fdiv", INSTR_FDIV, {InstrVar, InstrVar, InstrVar}},
    {"fneg", INSTR_FNEG, {InstrVar, InstrVar}},
    {"flt", INSTR_FLT, {InstrVar, InstrVar}},
    {"fle", INSTR_FLE, {InstrVar, InstrVar}},
    {"fgt", INSTR_FGT, {InstrVar, InstrVar}},
    {"fge", INSTR_FGE, {InstrVar, InstrVar}},
    {"feq", INSTR_FEQ, {InstrVar, InstrVar}},
    {"fne", INSTR_FNE, {InstrVar, InstrVar}},

    {"newlist", INSTR_NEWLIST, {InstrVar}},
    {"addtolist", INSTR_ADDTOLIST, {InstrVar, InstrVar}},
    {"getfromlist", INSTR_GETFROMLIST, {InstrVar, InstrVar, InstrVar}},

    /* guard: int */
    {"iadd", INSTR_IADD, {InstrVar, InstrVar, InstrVar}},
    {"isub", INSTR_ISUB, {InstrVar, InstrVar, InstrVar}},
    {"imul", INSTR_IMUL, {InstrVar, InstrVar, InstrVar}},
    {"idiv", INSTR_IDIV, {InstrVar, InstrVar, InstrVar}},
    {"ineg", INSTR_INEG, {InstrVar, InstrVar}},
    {"imod", INSTR_IMOD, {InstrVar, InstrVar, InstrVar}},
    {"ilt", INSTR_ILT, {InstrVar, InstrVar}},
    {"ile", INSTR_ILE, {InstrVar, InstrVar}},
    {"igt", INSTR_IGT, {InstrVar, InstrVar}},
    {"ige", INSTR_IGE, {InstrVar, InstrVar}},
    {"ieq", INSTR_IEQ, {InstrVar, InstrVar}},
    {"ine", INSTR_INE, {InstrVar, InstrVar}},

    {"iaddfunc", INSTR_IADDFUNC, {InstrVar, InstrVar, InstrVar}},
    {"isubfunc", INSTR_ISUBFUNC, {InstrVar, InstrVar, InstrVar}},
    {"imulfunc", INSTR_IMULFUNC, {InstrVar, InstrVar, InstrVar}},
    {"idivfunc", INSTR_IDIVFUNC, {InstrVar, InstrVar, InstrVar}},
    {"imodfunc", INSTR_IMODFUNC, {InstrVar, InstrVar, InstrVar}},
    {0}
  };
