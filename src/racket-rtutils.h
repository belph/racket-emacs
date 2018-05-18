#ifndef RE_RACKET_RTUTILS_H
#define RE_RACKET_RTUTILS_H

#include <re-config.h>
#include <scheme.h>
#include <emacs-module.h>

#include "racket-rt.h"
#include "conv.h"

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

void raise_emacs_exn(const char *msg);

Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f, Scheme_Object **exn);
Scheme_Object *_apply_func_catch_exceptions(Scheme_Object *f, int argc, Scheme_Object *argv[], Scheme_Object **exn);
Scheme_Object *extract_exn_message(Scheme_Object *v);

void init_racket_rtutils();

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

#endif // RE_RACKET_RTUTILS_H
