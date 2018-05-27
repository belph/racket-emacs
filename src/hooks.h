#ifndef RE_HOOKS_H
#define RE_HOOKS_H
#include <re-macros.h>
#include <emacs-module.h>
#include <string.h>

typedef enum {
  MODULE_INIT,
  POST_RACKET_INIT,
  POST_RACKETUTILS_INIT,
  NUM_HOOKS
} re_hook_type;

typedef int (re_hook_func)(emacs_env*);

typedef struct {
  re_hook_type type;
  re_hook_func *func;
  char dbg_name[128];
} re_hook_reginfo;

int register_hook(re_hook_reginfo*);
int run_hooks(re_hook_type, emacs_env *env);

#define RE_SETUP_HOOK(hook_name, hook_type, hook_func) \
  __attribute__((constructor)) \
  static void (re_setup_hook ## hook_func) () { \
    re_hook_reginfo rinfo = {            \
                             .type = hook_type, \
                             .func = hook_func,     \
                             .dbg_name = hook_name, \
    };                                   \
    dprintf("Registering hook: " hook_name "\n"); \
    register_hook(&rinfo);                        \
  }

#endif // RE_HOOKS_H
