# Architecture of SLIM

SLIM is composed of several modules.
Modules loosely depend on each other.

- *element*: contains fundamental data structures and functions.
- *loader*: contains a parser and an il-C translator.
- *verifier*: is an implementation of a model checker.
- *vm*: is an implementation of LMNtal.
- *ext*: contains extension callbacks for VM.
- *ffi*: is an interface to access an LMNtal compiler or JNI(Java Native Interface).

**Figure 1: SLIM's Architecture**
@image html slim_arch.svg
