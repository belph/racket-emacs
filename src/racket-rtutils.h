#ifndef RE_RACKET_RTUTILS_H
#define RE_RACKET_RTUTILS_H

#include <re-config.h>
#include <re-macros.h>
#include <scheme.h>
#include <emacs-module.h>

#include "racket-rt.h"
#include "conv.h"

typedef struct {
  Scheme_Primitive_Closure_Proc *proc;
  const char *proc_name;
  size_t primc;
  Scheme_Object **primv;
  emacs_env *env;
} emacs_mz_prim_runinfo;


emacs_value wrap_racket_value(emacs_env *env, Scheme_Object *value);
Scheme_Object *wrap_emacs_env(emacs_env *env);
Scheme_Object *wrap_emacs_value(emacs_env *env, emacs_value value, int rethrow);

emacs_env *get_current_emacs_env();
emacs_env *get_emacs_env(Scheme_Config *config);

void set_current_emacs_env(emacs_env *env);
void set_emacs_env(Scheme_Config *config, emacs_env *env);

Scheme_Config *config_with_env(Scheme_Config *config, emacs_env *env);
Scheme_Config *current_config_with_env(emacs_env *env);

Scheme_Object *with_env(emacs_env *env, Scheme_Object* (*worker_fun)(void*), Scheme_Object* (*jump_handler)(void*), void *data);
Scheme_Object *racket_safe_run_prim(emacs_mz_prim_runinfo *runinfo);

void raise_emacs_exn(const char *msg);

Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f, Scheme_Object **exn);
Scheme_Object *_apply_func_catch_exceptions(Scheme_Object *f, int argc, Scheme_Object *argv[], Scheme_Object **exn);
Scheme_Object *extract_exn_message(Scheme_Object *v);
Scheme_Object *expand_requires(emacs_env *env, Scheme_Object *v);
// val == NULL is the same as #f
Scheme_Object *racket_safe_dynamic_require(emacs_env *env, Scheme_Object *mod, Scheme_Object *val);

int init_racket_rtutils(emacs_env *eenv);

#define RETHROW_EMACS_ERROR(env)                \
  do {                                            \
    emacs_env *___eenv = env;                     \
    emacs_value _Qsym, _Qdata;                                          \
    if (___eenv->non_local_exit_get(___eenv, &_Qsym, &_Qdata) != emacs_funcall_exit_return) { \
      MZ_GC_UNREG();                                                    \
      scheme_signal_error("Emacs exception: '%a: %a", \
                          conv_emacs_symbol_to_scheme_symbol(___eenv, _Qsym), \
                          conv_emacs_string_to_scheme_string(___eenv, _Qdata)); \
    }                                                                   \
  } while (0)

#ifdef RE_DEBUG_BUILD

#define SCHEME_PRINT_STR_PORT(s, port) \
  do {                                                \
    char __s[] = s;                                   \
    Scheme_Object *__p = port;                        \
    scheme_write_byte_string(__s, STRLEN(__s), __p);  \
    scheme_flush_output(__p);                         \
  } while (0)

#define SCHEME_PRINT_STR(s) \
  do {                                                \
    char __s[] = s;                                   \
    Scheme_Config *__c = NULL;                        \
    Scheme_Object *__p = NULL;                        \
    MZ_GC_DECL_REG(2);                                \
    MZ_GC_VAR_IN_REG(0, __c);                         \
    MZ_GC_VAR_IN_REG(1, __p);                         \
    MZ_GC_REG();                                      \
    __c = scheme_current_config();                        \
    __p = scheme_get_param(__c, MZCONFIG_OUTPUT_PORT);    \
    scheme_write_byte_string(__s, STRLEN(__s), __p);  \
    scheme_flush_output(__p);                         \
  } while (0)

#define SCHEME_DISPLAY(o) \
  do {                                                \
    Scheme_Config *__c = NULL;                        \
    Scheme_Object *__p = NULL;                        \
    MZ_GC_DECL_REG(2);                                \
    MZ_GC_VAR_IN_REG(0, __c);                         \
    MZ_GC_VAR_IN_REG(1, __p);                         \
    MZ_GC_REG();                                      \
    __c = scheme_current_config();                        \
    __p = scheme_get_param(__c, MZCONFIG_OUTPUT_PORT);    \
    scheme_display(o, __p);                               \
    scheme_flush_output(__p);                         \
  } while (0)

#else // RE_DEBUG_BUILD

#define SCHEME_PRINT_STR_PORT(s, port) do {} while (0)
#define SCHEME_PRINT_STR(s) do {} while (0)
#define SCHEME_DISPLAY(o) do {} while (0)

#endif // RE_DEBUG_BUILD

#define SCHEME_DISPLAYLN(o) SCHEME_DISPLAY(o); SCHEME_PRINT_STR("\n")

#endif // RE_RACKET_RTUTILS_H
