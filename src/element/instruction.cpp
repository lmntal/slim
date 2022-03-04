/*
 * instruction.cpp - Intermediate code instructions
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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

#include "instruction.hpp"

const std::map<LmnInstruction, InstrSpec> instr_spec = {
  { INSTR_DEREF, { "deref", { InstrVar, InstrVar, InstrVar, InstrVar } } },
  { INSTR_DEREFATOM, { "derefatom", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_DEREFLINK, { "dereflink", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_FINDATOM, { "findatom", { InstrVar, InstrVar, ArgFunctor } } },
  { INSTR_FINDATOM2, { "findatom2", { InstrVar, InstrVar, InstrVar, ArgFunctor } } },

  { INSTR_LOCKMEM, { "lockmem", { InstrVar, InstrVar, String } } },
  { INSTR_ANYMEM, { "anymem", { InstrVar, InstrVar, InstrVar, String } } },

  { INSTR_GETMEM, { "getmem", { InstrVar, InstrVar, InstrVar, String } } },
  { INSTR_GETPARENT, { "getparent", { InstrVar, InstrVar } } },

  { INSTR_TESTMEM, { "testmem", { InstrVar, InstrVar } } },
  { INSTR_NORULES, { "norules", { InstrVar } } },
  { INSTR_NFREELINKS, { "nfreelinks", { InstrVar, InstrVar } } },
  { INSTR_NATOMS, { "natoms", { InstrVar, InstrVar } } },
  { INSTR_NATOMSINDIRECT, { "natomsindirect", { InstrVar, InstrVar } } },
  { INSTR_NMEMS, { "nmems", { InstrVar, InstrVar } } },
  { INSTR_EQMEM, { "eqmem", { InstrVar, InstrVar } } },
  { INSTR_NEQMEM, { "neqmem", { InstrVar, InstrVar } } },
  { INSTR_STABLE, { "stable", { InstrVar } } },

  { INSTR_FUNC, { "func", { InstrVar, ArgFunctor } } },
  { INSTR_NOTFUNC, { "notfunc", { InstrVar, ArgFunctor } } },
  { INSTR_EQATOM, { "eqatom", { InstrVar, InstrVar } } },
  { INSTR_NEQATOM, { "neqatom", { InstrVar, InstrVar } } },
  { INSTR_SAMEFUNC, { "samefunc", { InstrVar, InstrVar } } },

  { INSTR_DEREFFUNC, { "dereffunc", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_GETFUNC, { "getfunc", { InstrVar, InstrVar } } },
  { INSTR_LOADFUNC, { "loadfunc", { InstrVar, ArgFunctor } } },
  { INSTR_EQFUNC, { "eqfunc", { InstrVar, InstrVar } } },
  { INSTR_NEQFUNC, { "neqfunc", { InstrVar, InstrVar } } },

  { INSTR_REMOVEATOM, { "removeatom", { InstrVar, InstrVar, ArgFunctor } } },
  { INSTR_REMOVEATOM, { "removeatom", { InstrVar, InstrVar } } },
  { INSTR_NEWATOM, { "newatom", { InstrVar, InstrVar, ArgFunctor } } },
  { INSTR_NEWATOMINDIRECT, { "newatomindirect", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ENQUEUEATOM, { "enqueueatom", { InstrVar } } },
  { INSTR_DEQUEUEATOM, { "dequeueatom", { InstrVar } } },
  { INSTR_FREEATOM, { "freeatom", { InstrVar } } },
  { INSTR_ALTERFUNC, { "alterfunc", { InstrVar, ArgFunctor } } },

  { INSTR_TAILATOM, { "tailatom", { InstrVar, InstrVar } } },
  { INSTR_HEADATOM, { "headatom", { InstrVar, InstrVar } } },
  { INSTR_TAILATOMLIST, { "tailatomlist", { InstrVar, InstrVar } } },
  { INSTR_ATOMTAILATOM, { "atomtailatom", { InstrVar, InstrVar, InstrVar } } },

  { INSTR_CLEARLINK, { "clearlink", { InstrVar, InstrVar } } },

  { INSTR_FINDATOMP, { "findatomp", { InstrVar, InstrVar, ArgFunctor } } },
  { INSTR_SYNC, { "sync", {} } },

  { INSTR_ALLOCATOM, { "allocatom", { InstrVar, ArgFunctor } } },
  { INSTR_ALLOCATOMINDIRECT, { "allocatomindirect", { InstrVar, InstrVar } } },
  { INSTR_COPYATOM, { "copyatom", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ADDATOM, { "addatom", { InstrVar, InstrVar } } },

  { INSTR_REMOVEMEM, { "removemem", { InstrVar, InstrVar } } },
  { INSTR_NEWMEM, { "newmem", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ALLOCMEM, { "allocmem", { InstrVar } } },
  { INSTR_NEWROOT, { "newroot", { InstrVar, InstrVar, InstrVar, InstrVar } } },
  { INSTR_MOVECELLS, { "movecells", { InstrVar, InstrVar } } },
  { INSTR_ENQUEUEALLATOMS, { "enqueueallatoms", { InstrVar } } },
  { INSTR_FREEMEM, { "freemem", { InstrVar } } },
  { INSTR_ADDMEM, { "addmem", { InstrVar, InstrVar } } },
  { INSTR_ENQUEUEMEM, { "enqueuemem", { InstrVar } } },
  { INSTR_UNLOCKMEM, { "unlockmem", { InstrVar } } },
  { INSTR_SETMEMNAME, { "setmemname", { InstrVar, String } } },

  { INSTR_GETLINK, { "getlink", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_HYPERGETLINK, { "hypergetlink", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ALLOCLINK, { "alloclink", { InstrVar, InstrVar, InstrVar } } },

  { INSTR_NEWLINK, { "newlink", { InstrVar, InstrVar, InstrVar, InstrVar, InstrVar } } },
  { INSTR_RELINK, { "relink", { InstrVar, InstrVar, InstrVar, InstrVar, InstrVar } } },
  { INSTR_SWAPLINK, { "swaplink", { InstrVar, InstrVar, InstrVar, InstrVar } } },
  { INSTR_UNIFY, { "unify", { InstrVar, InstrVar, InstrVar, InstrVar, InstrVar } } },
  { INSTR_INHERITLINK, { "inheritlink", { InstrVar, InstrVar, InstrVar, InstrVar } } },
  { INSTR_UNIFYLINKS, { "unifylinks", { InstrVar, InstrVar, InstrVar } } },

  { INSTR_REMOVEPROXIES, { "removeproxies", { InstrVar } } },
  { INSTR_REMOVETOPLEVELPROXIES, { "removetoplevelproxies", { InstrVar } } },
  { INSTR_INSERTPROXIES, { "insertproxies", { InstrVar, InstrVar } } },
  { INSTR_REMOVETEMPORARYPROXIES, { "removetemporaryproxies", { InstrVar } } },

  { INSTR_LOADRULESET, { "loadruleset", { InstrVar, ArgRuleset } } },
  { INSTR_COPYRULES, { "copyrules", { InstrVar, InstrVar } } },
  { INSTR_CLEARRULES, { "clearrules", { InstrVar } } },
  { INSTR_LOADMODULE, { "loadmodule", { InstrVar, String } } },

  { INSTR_RECURSIVELOCK, { "recursivelock", { InstrVar } } },
  { INSTR_RECURSIVEUNLOCK, { "recursiveunlock", { InstrVar } } },
  { INSTR_COPYCELLS, { "copycells", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_DROPMEM, { "dropmem", { InstrVar } } },
  { INSTR_LOOKUPLINK, { "lookuplink", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_INSERTCONNECTORS, { "insertconnectors", { InstrVar, InstrVarList, InstrVar } } },
  { INSTR_INSERTCONNECTORSINNULL, { "insertconnectorsinnull", { InstrVar, InstrVarList } } },
  { INSTR_DELETECONNECTORS, { "deleteconnectors", { InstrVar, InstrVar } } },

  { INSTR_REACT, { "react", { InstrVar, InstrVarList, InstrVarList, InstrVarList } } },
  { INSTR_JUMP, { "jump", { Label, InstrVarList, InstrVarList, InstrVarList } } },
#ifdef KWBT_OPT
  { INSTR_COMMIT, { "commit", { String, LineNum, InstrVar } } },
#else
  { INSTR_COMMIT, { "commit", { String, LineNum } } },
#endif
  { INSTR_RESETVARS, { "resetvars", { InstrVarList, InstrVarList, InstrVarList } } },
  { INSTR_CHANGEVARS, { "changevars", { InstrVarList, InstrVarList, InstrVarList } } },
  { INSTR_SPEC, { "spec", { InstrVar, InstrVar } } },
  { INSTR_PROCEED, { "proceed", {} } },
  { INSTR_STOP, { "stop", {} } },

  { INSTR_LOOP, { "loop", { InstrList } } },
  { INSTR_RUN, { "run", { InstrList } } },
  { INSTR_NOT, { "not", { InstrList } } },
  { INSTR_INLINE, { "inline", { InstrVar, String, InstrVar } } },
  { INSTR_CALLBACK, { "callback", { InstrVar, InstrVar } } },
  { INSTR_UNIFYHLINKS, { "unifyhlinks", { InstrVar, InstrVar } } },
  { INSTR_FINDPROCCXT,
    { "findproccxt", { InstrVar, InstrVar, InstrVar, InstrVar, InstrVar, InstrVar } } },

  /* special */
  { INSTR_GROUP, { "group", { InstrList } } },
  { INSTR_BRANCH, { "branch", { InstrList } } },

  /* guard */
  { INSTR_EQGROUND, { "eqground", { InstrVar, InstrVar } } },
  { INSTR_NEQGROUND, { "neqground", { InstrVar, InstrVar } } },
  { INSTR_COPYHLGROUND, { "copyhlground", { InstrVar, InstrVar, InstrVar, InstrVarList } } },
  { INSTR_COPYHLGROUNDINDIRECT,
    { "copyhlgroundindirect", { InstrVar, InstrVar, InstrVar, InstrVarList } } },
  { INSTR_COPYGROUND, { "copyground", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_REMOVEGROUND, { "removeground", { InstrVar, InstrVar } } },
  { INSTR_REMOVEHLGROUND, { "removehlground", { InstrVar, InstrVar, InstrVarList } } },
  { INSTR_REMOVEHLGROUNDINDIRECT,
    { "removehlgroundindirect", { InstrVar, InstrVar, InstrVarList } } },
  { INSTR_FREEGROUND, { "freeground", { InstrVar } } },
  { INSTR_FREEHLGROUND, { "freehlground", { InstrVar, InstrVarList } } },
  { INSTR_FREEHLGROUNDINDIRECT, { "freehlgroundindirect", { InstrVar, InstrVarList } } },
  { INSTR_ISGROUND, { "isground", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ISHLGROUND, { "ishlground", { InstrVar, InstrVar, InstrVar, InstrVarList } } },
  { INSTR_ISHLGROUNDINDIRECT,
    { "ishlgroundindirect", { InstrVar, InstrVar, InstrVar, InstrVarList } } },
  { INSTR_ISUNARY, { "isunary", { InstrVar } } },
  { INSTR_ISINT, { "isint", { InstrVar } } },
  { INSTR_ISINTFUNC, { "isintfunc", { InstrVar } } },

  { INSTR_ISFLOAT, { "isfloat", { InstrVar } } },
  { INSTR_ISSTRING, { "isstring", { InstrVar } } },

  { INSTR_FLOAT2INT, { "float2int", { InstrVar, InstrVar } } },
  { INSTR_INT2FLOAT, { "int2float", { InstrVar, InstrVar } } },
  { INSTR_FLOAT2INTFUNC, { "float2intfunc", { InstrVar, InstrVar } } },
  { INSTR_INT2FLOATFUNC, { "int2floatfunc", { InstrVar, InstrVar } } },

  { INSTR_UNIQ, { "uniq", { InstrVarList } } },

  /* guard: hyperlink */
  { INSTR_NEWHLINK, { "newhlink", { InstrVar } } },
  { INSTR_NEWHLINKWITHATTR, { "newhlinkwithattr", { InstrVar, InstrVar } } },
  { INSTR_NEWHLINKWITHATTRINDIRECT, { "newhlinkwithattrindirect", { InstrVar, InstrVar } } },
  { INSTR_MAKEHLINK, { "makehlink", { InstrVar } } },
  { INSTR_ISHLINK, { "ishlink", { InstrVar } } },
  { INSTR_GETATTRATOM, { "getattratom", { InstrVar, InstrVar } } },
  { INSTR_GETNUM, { "getnum", { InstrVar, InstrVar } } },

  /* guard: float */
  { INSTR_FADD, { "fadd", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_FSUB, { "fsub", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_FMUL, { "fmul", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_FDIV, { "fdiv", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_FNEG, { "fneg", { InstrVar, InstrVar } } },
  { INSTR_FLT, { "flt", { InstrVar, InstrVar } } },
  { INSTR_FLE, { "fle", { InstrVar, InstrVar } } },
  { INSTR_FGT, { "fgt", { InstrVar, InstrVar } } },
  { INSTR_FGE, { "fge", { InstrVar, InstrVar } } },
  { INSTR_FEQ, { "feq", { InstrVar, InstrVar } } },
  { INSTR_FNE, { "fne", { InstrVar, InstrVar } } },

  { INSTR_NEWLIST, { "newlist", { InstrVar } } },
  { INSTR_ADDTOLIST, { "addtolist", { InstrVar, InstrVar } } },
  { INSTR_GETFROMLIST, { "getfromlist", { InstrVar, InstrVar, InstrVar } } },

  /* guard: int */
  { INSTR_IADD, { "iadd", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ISUB, { "isub", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_IMUL, { "imul", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_IDIV, { "idiv", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_INEG, { "ineg", { InstrVar, InstrVar } } },
  { INSTR_IMOD, { "imod", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ILT, { "ilt", { InstrVar, InstrVar } } },
  { INSTR_ILE, { "ile", { InstrVar, InstrVar } } },
  { INSTR_IGT, { "igt", { InstrVar, InstrVar } } },
  { INSTR_IGE, { "ige", { InstrVar, InstrVar } } },
  { INSTR_IEQ, { "ieq", { InstrVar, InstrVar } } },
  { INSTR_INE, { "ine", { InstrVar, InstrVar } } },
  { INSTR_IAND, { "iand", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_IOR, { "ior", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_IXOR, { "ixor", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ISAL, { "isal", { InstrVar, InstrVar, InstrVar } } },

  { INSTR_ILTFUNC, { "iltfunc", { InstrVar, InstrVar } } },
  { INSTR_ILEFUNC, { "ilefunc", { InstrVar, InstrVar } } },
  { INSTR_IGTFUNC, { "igtfunc", { InstrVar, InstrVar } } },
  { INSTR_IGEFUNC, { "igefunc", { InstrVar, InstrVar } } },

  { INSTR_IADDFUNC, { "iaddfunc", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_ISUBFUNC, { "isubfunc", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_IMULFUNC, { "imulfunc", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_IDIVFUNC, { "idivfunc", { InstrVar, InstrVar, InstrVar } } },
  { INSTR_IMODFUNC, { "imodfunc", { InstrVar, InstrVar, InstrVar } } },

  { INSTR_GETCLASS, { "getclass", { InstrVar, InstrVar } } },
  { INSTR_SUBCLASS, { "subclass", { InstrVar, InstrVar } } },

  /* etc */
  { INSTR_CELLDUMP, { "celldump", {} } }
};

int get_instr_id(const char *name) {
  for (auto &p : instr_spec)
    if (!strcmp(name, p.second.op_str))
      return p.first;
  return -1;
}
