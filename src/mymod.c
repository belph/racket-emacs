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
#include "hooks.h"

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

int emacs_module_init (struct emacs_runtime *ert) {
  fprintf(stderr, "Running startup\n");
  emacs_env *env = ert->get_environment (ert);
  racket_main();
  emacs_value fun = env->make_function(env, 1, 2, Feval_racket_file, "Evaluate the given racket file", NULL);
  bind_function(env, "eval-racket-file", fun);
  run_hooks(MODULE_INIT, env);
  EMACS_CHECK_EXIT(env, -1);
  provide(env, "libracketemacs");

  /* loaded successfully */
  return 0;
}
