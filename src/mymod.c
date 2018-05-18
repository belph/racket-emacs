#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <re-config.h>
#include <emacs-module.h>
#include <scheme.h>
#include <ctype.h>

#include "racket-rt.h"
#include "emacs-c-utils.h"
#include "ffi-utils.h"

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

int emacs_module_init (struct emacs_runtime *ert) {
  fprintf(stderr, "Running startup\n");
  emacs_env *env = ert->get_environment (ert);
  racket_main();
  emacs_value fun = env->make_function(env, 1, 2, Feval_racket_file, "Evaluate the given racket file", NULL);
  RACKET_INIT_RET(env, -1);
  bind_function (env, "eval-racket-file", fun);
  register_ffi_utils_emacs_functions(env);
  provide (env, "libracketemacs");

  /* loaded successfully */
  return 0;
}
