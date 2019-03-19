#ifndef LMN_MC_STACK_MACRO_H
#define LMN_MC_STACK_MACRO_H

#ifdef PROFILE
#define pop_stack(List)                                                        \
  do {                                                                         \
    State *pop = (State *)vec_pop(List);                                       \
    if (pop->is_on_stack())                                                      \
     pop->unset_on_stack();                                                     \
    if (lmn_env.profile_level >= 3) {                                          \
      profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));         \
    }                                                                          \
  } while (0)
#define put_stack(List, St)                                                    \
  do {                                                                         \
    if (lmn_env.profile_level >= 3) {                                          \
      profile_add_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));            \
    }                                                                          \
    vec_push((List), (vec_data_t)(St));                                        \
  } while (0)
#define pop_deq(Deq, Dir)                                                      \
  do {                                                                         \
    if (Dir) {                                                                 \
      deq_pop_tail(Deq);                                                       \
    } else {                                                                   \
      deq_pop_head(Deq);                                                       \
    }                                                                          \
    if (lmn_env.profile_level >= 3) {                                          \
      profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));         \
    }                                                                          \
  } while (0)
#define push_deq(List, St, Dir)                                                \
  do {                                                                         \
    if (lmn_env.profile_level >= 3) {                                          \
      profile_add_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));            \
    }                                                                          \
    if (Dir) {                                                                 \
      (List)->push_tail((vec_data_t)(St));                                 \
    } else {                                                                   \
      (List)->push_head((vec_data_t)(St));                                 \
    }                                                                          \
  } while (0)
#define EXECUTE_PROFILE_START()                                                \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(Node));              \
    profile_start_timer(PROFILE_TIME__ACTIVE_FOR_IDLE_PROF);                   \
  }
#define EXECUTE_PROFILE_FINISH()                                               \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_finish_timer(PROFILE_TIME__ACTIVE_FOR_IDLE_PROF);                  \
  }
#define ADD_OPEN_PROFILE(M)                                                    \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_add_space(PROFILE_SPACE__OPEN_LIST, M);                            \
  }
#else
#define EXECUTE_PROFILE_START()
#define EXECUTE_PROFILE_FINISH()
#define ADD_OPEN_PROFILE(M)
#define pop_stack(List)                                                        \
  do {                                                                         \
    State *pop = (State *)vec_pop(List);                                       \
    if (pop->is_on_stack())                                                      \
     pop->unset_on_stack();                                                     \
  } while (0)
#define put_stack(List, St) vec_push((List), (vec_data_t)(St))
#define pop_deq(Deq, Dir)                                                      \
  do {                                                                         \
    if (Dir) {                                                                 \
      deq_pop_tail(Deq);                                                       \
    } else {                                                                   \
      (State *)deq_pop_head(Deq);                                              \
    }                                                                          \
  } while (0)
#define push_deq(List, St, Dir)                                                \
  do {                                                                         \
    if (Dir) {                                                                 \
      (List)->push_tail((vec_data_t)(St));                                 \
    } else {                                                                   \
      (List)->push_head((vec_data_t)(St));                                 \
    }                                                                          \
  } while (0)

#endif

#endif