/*****************************************************************************
 * Copyright (C) 2018 by Philip Blair                                        *
 *                                                                           *
 * This file is part of racket-emacs.                                        *
 *                                                                           *
 *   racket-emacs is free software: you can redistribute it and/or modify it *
 *   under the terms of the GNU Lesser General Public License as published   *
 *   by the Free Software Foundation, either version 3 of the License, or    *
 *   (at your option) any later version.                                     *
 *                                                                           *
 *   racket-emacs is distributed in the hope that it will be useful,         *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 *   GNU Lesser General Public License for more details.                     *
 *                                                                           *
 *   You should have received a copy of the GNU Lesser General Public        *
 *   License along with racket-emacs.  If not, see                           *
 *   <http://www.gnu.org/licenses/>.                                         *
 *****************************************************************************/
#include <stdlib.h>

#include <re-config.h>
#include <scheme.h>
#include <emacs-module.h>

#include "emacs-c-utils.h"
#include "conv.h"
#include "racket-rt.h"

// A large portion of this file is taken from VIM's mzscheme (i.e. Racket) module,
// as it addresses many nuances of portability and alternate Racket configurations.

#define STRLEN(x) ((sizeof(x) / sizeof(char)) - 1)

#ifdef RKT_NEEDS_TLS_SPACE
#if defined(_MSC_VER)
static __declspec(thread) void *tls_space;
extern intptr_t _tls_index;
#elif defined(__MINGW32__)
static __thread void *tls_space;
extern intptr_t _tls_index;
#else
static thread_local void *tls_space;
static intptr_t _tls_index = 0;
#endif // defined(_MSC_VER)
#endif // RKT_NEEDS_TLS_SPACE

typedef struct {
  char *filename;
  Scheme_Object **res;
} filerun_info;

Scheme_Type mz_emacs_type;
Scheme_Type mz_emacs_env_type;

static int emacs_env_param;
static bool initialized = false;
static bool load_base_module_failed = false;

/* global environment */
static Scheme_Env    *environment = NULL;
/* output/error handlers */
static Scheme_Object *curout = NULL;
static Scheme_Object *curerr = NULL;
/* exn:vim exception */
static Scheme_Object *exn_catching_apply = NULL;
static Scheme_Object *exn_p = NULL;
static Scheme_Object *exn_message = NULL;
static Scheme_Object *emacs_exn = NULL; /* Emacs Error exception */

#if !defined(MZ_PRECISE_GC) || MZSCHEME_VERSION_MAJOR < 400
static void *stack_base = NULL;
#endif


static void init_exn_catching_apply(void);
static Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object*, Scheme_Object**);
static Scheme_Object* extract_exn_message(Scheme_Object*);
static void register_emacs_exn(void);
void raise_emacs_exn(const char*);

#ifdef MZ_PRECISE_GC

static int wrapped_emacs_env_size_proc(void *obj) {
  return gcBYTES_TO_WORDS(sizeof(emacs_mz_env));
}

static int wrapped_emacs_env_mark_proc(void *obj) {
  return wrapped_emacs_env_size_proc(obj);
}

static int wrapped_emacs_env_fixup_proc(void *obj) {
  return wrapped_emacs_env_size_proc(obj);
}

static int wrapped_emacs_value_size_proc(void *obj) {
  return gcBYTES_TO_WORDS(sizeof(emacs_mz_value));
}

static int wrapped_emacs_value_mark_proc(void *obj) {
  return wrapped_emacs_value_size_proc(obj);
}

static int wrapped_emacs_value_fixup_proc(void *obj) {
  return wrapped_emacs_value_size_proc(obj);
}

#endif // MZ_PRECISE_GC

static emacs_env *get_current_emacs_env() {
  Scheme_Config *config;
  Scheme_Object *obj;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, config);
  MZ_GC_VAR_IN_REG(1, obj);
  MZ_GC_REG();
  config = scheme_current_config();
  obj = scheme_get_param(config, emacs_env_param);
  if (SCHEME_EMACSENVP(obj)) {
    MZ_GC_UNREG();
    return SCHEME_EMACSENV_DEREF(obj);
  }
  MZ_GC_UNREG();
  raise_emacs_exn("Emacs environment parameter was required but undefined!");
  // Won't reach
  return NULL;
}

static void wrapped_emacs_value_finalizer(void *p, void *data) {
  if (SCHEME_EMACSVALUEP(p)) {
    emacs_env *env = get_current_emacs_env();
    env->free_global_ref(env, SCHEME_EMACSVALUE_DEREF(p));
  }
}

Scheme_Object *wrap_emacs_value(emacs_env *env, emacs_value value) {
  emacs_mz_value *self = NULL;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, self);
  MZ_GC_REG();
  self = scheme_malloc_fail_ok(scheme_malloc_tagged, sizeof(emacs_mz_value));
  memset(self, 0, sizeof(emacs_mz_value));
  MZ_GC_CHECK();
  self->value = env->make_global_ref(env, value);
  self->so.type = mz_emacs_type;
  scheme_register_finalizer(self, wrapped_emacs_value_finalizer, NULL, NULL, NULL);
  MZ_GC_UNREG();
  return (Scheme_Object*)self;
}

static Scheme_Object *wrap_emacs_env(emacs_env *env) {
  emacs_mz_env *self = NULL;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, self);
  MZ_GC_REG();
  self = scheme_malloc_fail_ok(scheme_malloc_tagged, sizeof(emacs_mz_env));
  memset(self, 0, sizeof(emacs_mz_env));
  MZ_GC_CHECK();
  self->env = env;
  self->so.type = mz_emacs_env_type;
  MZ_GC_UNREG();
  return (Scheme_Object*)self;
}

emacs_value wrap_racket_value(emacs_env *env, Scheme_Object *value) {
  scheme_dont_gc_ptr(value);
  return env->make_user_ptr(env, scheme_gc_ptr_ok, value);
}



static int racket_env_main(Scheme_Env *env, int argc, char *argv[]) {
#ifdef TRAMPOLINED_RACKET_STARTUP
  environment = env; // Use racket-provided environment
#else
#ifdef MZ_PRECISE_GC
  Scheme_Object   *dummy = NULL;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, dummy);

  stack_base = &__gc_var_stack__;
#else
  int dummy = 0;
  stack_base = (void *)&dummy;
#endif // MZ_PRECISE_GC
#endif // TRAMPOLINED_RACKET_STARTUP
  return 0;
}

// Begins the Racket interpreter (trampolining if necessary) 
int racket_main() {
  int argc = 0;
  char *argv = NULL;
  emacs_env_param = scheme_new_param();

#ifdef RKT_NEEDS_TLS_SPACE
  scheme_register_tls_space(&tls_space, _tls_index);
#endif // RKT_NEEDS_TLS_SPACE
#ifdef TRAMPOLINED_RACKET_STARTUP
  return scheme_main_setup(true, racket_env_main, argc, &argv);
#else
  return racket_env_main(NULL, argc, &argv);
#endif // TRAMPOLINED_RACKET_STARTUP
}

static inline void racket_setup_collections() {
#if MZSCHEME_VERSION_MAJOR >= 299
  {
    Scheme_Object *coll_path = NULL;
    char *s;

    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, coll_path);
    MZ_GC_REG();
    /* workaround for dynamic loading on windows */
    s = getenv("PLTCOLLECTS");
    if (s != NULL) {
      coll_path = scheme_make_path(s);
      MZ_GC_CHECK();
    }
#  ifdef RACKET_COLLECTS_DIR
    if (coll_path == NULL) {
      coll_path = scheme_make_path(RACKET_COLLECTS_DIR);
      MZ_GC_CHECK();
    }
#  endif // RACKET_COLLECTS_DIR
    if (coll_path != NULL) {
      scheme_set_collects_path(coll_path);
      MZ_GC_CHECK();
    }
    MZ_GC_UNREG();
  }
#elif defined(RACKET_COLLECTS_DIR)
  {
    Scheme_Object *coll_string = NULL;
    Scheme_Object *coll_pair = NULL;
    Scheme_Config *config = NULL;

    MZ_GC_DECL_REG(3);
    MZ_GC_VAR_IN_REG(0, coll_string);
    MZ_GC_VAR_IN_REG(1, coll_pair);
    MZ_GC_VAR_IN_REG(2, config);
    MZ_GC_REG();
    coll_string = scheme_make_byte_string(RACKET_COLLECTS_DIR);
    MZ_GC_CHECK();
    coll_pair = scheme_make_pair(coll_string, scheme_null);
    MZ_GC_CHECK();
    config = scheme_current_config();
    MZ_GC_CHECK();
    scheme_set_param(config, MZCONFIG_COLLECTION_PATHS, coll_pair);
    MZ_GC_CHECK();
    MZ_GC_UNREG();
  }
#endif // MZSCHEME_VERSION_MAJOR
}

static inline void racket_setup_config() {
#if MZSCHEME_VERSION_MAJOR >= 600
  {
    Scheme_Object *config_path = NULL;
    char *s;

    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, config_path);
    MZ_GC_REG();
    /* workaround for dynamic loading on windows */
    s = getenv("PLTCONFIGDIR");
    if (s != NULL) {
      config_path = scheme_make_path(s);
      MZ_GC_CHECK();
    }
#ifdef RACKET_CONFIG_DIR
    if (config_path == NULL) {
      config_path = scheme_make_path(RACKET_CONFIG_DIR);
      MZ_GC_CHECK();
    }
#endif
    if (config_path != NULL) {
      scheme_set_config_path(config_path);
      MZ_GC_CHECK();
    }
    MZ_GC_UNREG();
  }
#endif
}

static inline void racket_setup_sandbox() {
#ifdef HAVE_SANDBOX
  {
    Scheme_Object	*make_security_guard = NULL;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, make_security_guard);
    MZ_GC_REG();

#if MZSCHEME_VERSION_MAJOR < 400
    {
      Scheme_Object	*make_security_guard_symbol = NULL;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, make_security_guard_symbol);
      MZ_GC_REG();
      make_security_guard_symbol = scheme_intern_symbol("make-security-guard");
      MZ_GC_CHECK();
      make_security_guard = scheme_lookup_global(make_security_guard_symbol, environment);
      MZ_GC_UNREG();
    }
#else
    make_security_guard = scheme_builtin_value("make-security-guard");
    MZ_GC_CHECK();
#endif

    /* setup sandbox guards */
    if (make_security_guard != NULL)
      {
	Scheme_Object   *args[3] = {NULL, NULL, NULL};
	Scheme_Object   *guard = NULL;
	Scheme_Config   *config = NULL;
	MZ_GC_DECL_REG(5);
	MZ_GC_ARRAY_VAR_IN_REG(0, args, 3);
	MZ_GC_VAR_IN_REG(3, guard);
	MZ_GC_VAR_IN_REG(4, config);
	MZ_GC_REG();
	config = scheme_current_config();
	MZ_GC_CHECK();
	args[0] = scheme_get_param(config, MZCONFIG_SECURITY_GUARD);
	MZ_GC_CHECK();
	args[1] = scheme_make_prim_w_arity(sandbox_file_guard,
					   "sandbox-file-guard", 3, 3);
	args[2] = scheme_make_prim_w_arity(sandbox_network_guard,
					   "sandbox-network-guard", 4, 4);
	guard = scheme_apply(make_security_guard, 3, args);
	MZ_GC_CHECK();
	scheme_set_param(config, MZCONFIG_SECURITY_GUARD, guard);
	MZ_GC_CHECK();
	MZ_GC_UNREG();
      }
    MZ_GC_UNREG();
  }
#endif
}

static Scheme_Object *closed_dynamic_require(int argc, Scheme_Object **argv, Scheme_Object *prim) {
  Scheme_Object *a[2] = {NULL, NULL};
  Scheme_Object *ret;
  MZ_GC_DECL_REG(3);
  MZ_GC_ARRAY_VAR_IN_REG(0, a, 2);
  MZ_GC_VAR_IN_REG(2, ret);
  MZ_GC_REG();
  a[0] = SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  a[1] = SCHEME_PRIM_CLOSURE_ELS(prim)[1];
  MZ_GC_CHECK();
  ret = scheme_dynamic_require(2, a);
  MZ_GC_UNREG();
  return ret;
}

static Scheme_Object *load_base_module(void *data) {
  scheme_namespace_require(scheme_intern_symbol((char *)data));
  return scheme_null;
}

static Scheme_Object *load_base_module_on_error(void *data) {
  load_base_module_failed = true;
  return scheme_null;
}

/* This function sets up the Racket runtime environment.
 * It is called once, before any user code is run.
 */
static int racket_startup() {
#ifndef TRAMPOLINED_RACKET_STARTUP
  scheme_set_stack_base(stack_base, 1);
  /* in newer versions of precise GC the initial env has been created */
  environment = scheme_basic_env();
#endif
  MZ_REGISTER_STATIC(environment);
  MZ_REGISTER_STATIC(curout);
  MZ_REGISTER_STATIC(curerr);
  MZ_REGISTER_STATIC(exn_catching_apply);
  MZ_REGISTER_STATIC(exn_p);
  MZ_REGISTER_STATIC(exn_message);
  MZ_REGISTER_STATIC(emacs_exn);

  MZ_GC_CHECK();

  racket_setup_collections();
  racket_setup_config();

#if MZSCHEME_VERSION_MAJOR >= 400
  scheme_init_collection_paths(environment, scheme_null);
#endif

  /*
   * versions 4.x do not provide Scheme bindings by default
   * we need to add them explicitly
   */
  {
    /* use error handler to avoid abort */
    scheme_dynamic_wind(NULL, load_base_module, NULL,
			load_base_module_on_error, "racket/base");
    if (load_base_module_failed) {
      load_base_module_failed = false;
      scheme_dynamic_wind(NULL, load_base_module, NULL,
			  load_base_module_on_error, "scheme/base");
      if (load_base_module_failed) {
	return -1;
      }
    }
  }

  register_emacs_exn();
  init_exn_catching_apply();
  /* redirect output */
  //scheme_console_output = do_output;
  //scheme_console_printf = do_printf;
  
  racket_setup_sandbox();

  mz_emacs_type = scheme_make_type("<emacs-value>");
  MZ_GC_CHECK();
  mz_emacs_env_type = scheme_make_type("<emacs-env>");
  MZ_GC_CHECK();
#ifdef MZ_PRECISE_GC
  GC_register_traversers(mz_emacs_type,
			 wrapped_emacs_value_size_proc,
			 wrapped_emacs_value_mark_proc,
			 wrapped_emacs_value_fixup_proc,
			 true, true);
  GC_register_traversers(mz_emacs_env_type,
			 wrapped_emacs_env_size_proc,
			 wrapped_emacs_env_mark_proc,
			 wrapped_emacs_env_fixup_proc,
			 true, true);
#endif
  return 0;
}

static int racket_init(emacs_env *emacs_env) {
  if (!initialized) {
    if (load_base_module_failed || racket_startup()) {
      emacs_value Qthrow_tag = emacs_env->intern(emacs_env, "racket-emacs-init");
      char msg[] = "Failed to initialize racket/base module.";
      emacs_value Qthrow_value = emacs_env->make_string(emacs_env, msg, (sizeof(msg) / sizeof(char)) - 1);
      if (emacs_env->non_local_exit_check(emacs_env) != emacs_funcall_exit_return) {
	return -1;
      }
      emacs_env->non_local_exit_throw(emacs_env, Qthrow_tag, Qthrow_value);
      return -1;
    }
    initialized = true;
  }
  {
    Scheme_Config	*config = NULL;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, config);
    MZ_GC_REG();
    config = scheme_current_config();
    MZ_GC_CHECK();
    /* recreate ports each call effectively clearing these ones */
    curout = scheme_make_byte_string_output_port();
    MZ_GC_CHECK();
    curerr = scheme_make_byte_string_output_port();
    MZ_GC_CHECK();
    scheme_set_param(config, MZCONFIG_OUTPUT_PORT, curout);
    MZ_GC_CHECK();
    scheme_set_param(config, MZCONFIG_ERROR_PORT, curerr);
    MZ_GC_UNREG();
  }

  return 0;
}

static void prepare_env(void *envptr) {
  emacs_mz_env_ctxt *ctxt = (emacs_mz_env_ctxt*)envptr;
  Scheme_Config *config = NULL;
  Scheme_Object *value = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, config);
  MZ_GC_VAR_IN_REG(1, value);
  MZ_GC_REG();
  config = scheme_current_config();
  MZ_GC_CHECK();
  value = scheme_get_param(config, emacs_env_param);
  scheme_dont_gc_ptr(value);
  ctxt->pre_env = value;
  value = wrap_emacs_env(ctxt->cur_env);
  scheme_set_param(config, emacs_env_param, value);
}

static Scheme_Object *run_worker(void *envptr) {
  emacs_mz_env_ctxt *ctxt = (emacs_mz_env_ctxt*)envptr;
  return ctxt->worker_fun(ctxt->data);
}

static Scheme_Object *run_jump_handler(void *envptr) {
  emacs_mz_env_ctxt *ctxt = (emacs_mz_env_ctxt*)envptr;
  return ctxt->jump_handler(ctxt->data);
}

static void teardown_env(void *envptr) {
  emacs_mz_env_ctxt *ctxt = (emacs_mz_env_ctxt*)envptr;
  Scheme_Config *config = NULL;
  Scheme_Object *value = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, config);
  MZ_GC_VAR_IN_REG(1, value);
  MZ_GC_REG();
  config = scheme_current_config();
  MZ_GC_CHECK();
  value = ctxt->pre_env;
  scheme_gc_ptr_ok(value);
  scheme_set_param(config, emacs_env_param, value);
}

static void *with_env(emacs_env *env, Scheme_Object* (*worker_fun)(void*), Scheme_Object* (*jump_handler)(void*), void *data) {
  emacs_mz_env_ctxt ctxt;
  ctxt.cur_env = env;
  ctxt.worker_fun = worker_fun;
  ctxt.jump_handler = jump_handler;
  ctxt.data = data;
  return scheme_dynamic_wind(prepare_env, run_worker, teardown_env, run_jump_handler, &ctxt);
}

static Scheme_Object *do_eval_racket_file(void *frinfo_ptr) {
  filerun_info *frinfo = (filerun_info*)frinfo_ptr;
  char *filename = frinfo->filename;
  Scheme_Object   *value = NULL;
  Scheme_Object   *exn = NULL;
  Scheme_Object   *prim = NULL;
  Scheme_Object *a[2] = {NULL, NULL};
  MZ_GC_DECL_REG(5);
  MZ_GC_VAR_IN_REG(0, value);
  MZ_GC_VAR_IN_REG(1, exn);
  MZ_GC_VAR_IN_REG(2, prim);
  MZ_GC_ARRAY_VAR_IN_REG(2, a, 3);
  MZ_GC_REG();

  a[0] = scheme_make_pair(scheme_intern_symbol("file"),
			  scheme_make_pair(scheme_make_utf8_string(filename),
					   scheme_null));
  a[1] = scheme_make_false();
  
  prim = scheme_make_prim_closure_w_arity(closed_dynamic_require, 2, a, "closed-dynamic-require", 0, 0);
  MZ_GC_CHECK();
  value = _apply_thunk_catch_exceptions(prim, &exn);
  MZ_GC_CHECK();

  if (!value) {
    *(frinfo->res) = scheme_make_false();
    emacs_env *emacs_env = get_current_emacs_env();
    value = extract_exn_message(exn);
    emacs_value Qthrow_tag = emacs_env->intern(emacs_env, "racket-emacs");
    emacs_value Qthrow_value;
    if (value) {
      Qthrow_value = conv_scheme_string_to_emacs_string(emacs_env, value);
      MZ_GC_CHECK();
    } else {
      Qthrow_value = emacs_env->intern(emacs_env, "nil");
    }
    if (emacs_env->non_local_exit_check(emacs_env) == emacs_funcall_exit_return) {
      emacs_env->non_local_exit_throw(emacs_env, Qthrow_tag, Qthrow_value);
    }
    MZ_GC_UNREG();
    return NULL;
  }
  *(frinfo->res) = value;
  MZ_GC_UNREG();
  return NULL;
}

static Scheme_Object *impossible(void *ptr) {
  abort();
  return NULL;
}

emacs_value Feval_racket_file(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  char *filename = emacs_string_to_c_string(env, argv[0]);
  EMACS_CHECK_EXIT(env, NULL);
  if (racket_init(env)) {
    EMACS_CHECK_EXIT(env, NULL);
    emacs_value Qthrow_tag = env->intern(env, "racket-emacs");
    char msg[] = "Failed to initialize Racket runtime";
    emacs_value Qthrow_value = env->make_string(env, msg, STRLEN(msg));
    env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
    return NULL;
  }
  Scheme_Object *res = NULL;
  filerun_info frinfo = { filename, &res };
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, res);
  MZ_GC_REG();
  with_env(env, do_eval_racket_file, impossible, &frinfo);
  MZ_GC_UNREG();
  return env->intern(env, "nil");
}

static int eval_racket_with_exn_handling(emacs_env *emacs_env, void *data, Scheme_Closed_Prim *what, Scheme_Object **ret) {
  Scheme_Object   *value = NULL;
  Scheme_Object   *exn = NULL;
  Scheme_Object   *prim = NULL;

  MZ_GC_DECL_REG(3);
  MZ_GC_VAR_IN_REG(0, value);
  MZ_GC_VAR_IN_REG(1, exn);
  MZ_GC_VAR_IN_REG(2, prim);
  MZ_GC_REG();

  prim = scheme_make_closed_prim_w_arity(what, data, "racket-emacs", 0, 0);
  MZ_GC_CHECK();
  value = _apply_thunk_catch_exceptions(prim, &exn);
  MZ_GC_CHECK();

  if (!value) {
    value = extract_exn_message(exn);
    emacs_value Qthrow_tag = emacs_env->intern(emacs_env, "racket-emacs");
    emacs_value Qthrow_value;
    if (value) {
      Qthrow_value = conv_scheme_string_to_emacs_string(emacs_env, value);
      MZ_GC_CHECK();
    } else {
      Qthrow_value = emacs_env->intern(emacs_env, "nil");
    }
    if (emacs_env->non_local_exit_check(emacs_env) == emacs_funcall_exit_return) {
      emacs_env->non_local_exit_throw(emacs_env, Qthrow_tag, Qthrow_value);
    }
    MZ_GC_UNREG();
    return -1;
  }

  if (ret != NULL) {	/* if pointer to retval supported give it up */
    *ret = value;
  } else if (!SCHEME_VOIDP(value)) { /* Print any result, as long as it's not a void */
    scheme_display(value, curout);  /* Send to stdout-vim */
    MZ_GC_CHECK();
  }

  //do_flush();
  MZ_GC_UNREG();
  return 0;
}


static void register_emacs_exn() {
  int nc = 0;
  int i;
  Scheme_Object   *struct_exn = NULL;
  Scheme_Object   *exn_name = NULL;

  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, struct_exn);
  MZ_GC_VAR_IN_REG(1, exn_name);
  MZ_GC_REG();

  exn_name = scheme_intern_symbol("exn:emacs");
  MZ_GC_CHECK();
  struct_exn = scheme_builtin_value("struct:emacs");
  MZ_GC_CHECK();

  if (emacs_exn == NULL) {
    emacs_exn = scheme_make_struct_type(exn_name,
					struct_exn, NULL, 0, 0, NULL, NULL
#if MZSCHEME_VERSION_MAJOR >= 299
					, NULL
#endif
					);
  }


  {
    Scheme_Object   **tmp = NULL;
    Scheme_Object   *exn_names[5] = {NULL, NULL, NULL, NULL, NULL};
    Scheme_Object   *exn_values[5] = {NULL, NULL, NULL, NULL, NULL};
    MZ_GC_DECL_REG(6);
    MZ_GC_ARRAY_VAR_IN_REG(0, exn_names, 5);
    MZ_GC_ARRAY_VAR_IN_REG(3, exn_values, 5);
    MZ_GC_REG();

    tmp = scheme_make_struct_names(exn_name, scheme_null, 0, &nc);
    memmove(exn_names, tmp, nc * sizeof(Scheme_Object *));
    MZ_GC_CHECK();

    tmp = scheme_make_struct_values(emacs_exn, exn_names, nc, 0);
    memmove(exn_values, tmp, nc * sizeof(Scheme_Object *));
    MZ_GC_CHECK();

    for (i = 0; i < nc; i++)
      {
	scheme_add_global_symbol(exn_names[i],
				 exn_values[i], environment);
	MZ_GC_CHECK();
      }
    MZ_GC_UNREG();
  }
  MZ_GC_UNREG();
}

/*
 * raise exn:emacs, may be with additional info string
 */
void raise_emacs_exn(const char *add_info) {
  char	    *fmt = "Emacs error: ~a";
  Scheme_Object   *argv[2] = {NULL, NULL};
  Scheme_Object   *exn = NULL;
  Scheme_Object   *byte_string = NULL;

  MZ_GC_DECL_REG(5);
  MZ_GC_ARRAY_VAR_IN_REG(0, argv, 2);
  MZ_GC_VAR_IN_REG(3, exn);
  MZ_GC_VAR_IN_REG(4, byte_string);
  MZ_GC_REG();

  if (add_info != NULL) {
    char *c_string = NULL;
    Scheme_Object *info = NULL;

    MZ_GC_DECL_REG(3);
    MZ_GC_VAR_IN_REG(0, c_string);
    MZ_GC_VAR_IN_REG(2, info);
    MZ_GC_REG();

    info = scheme_make_byte_string(add_info);
    MZ_GC_CHECK();
    c_string = scheme_format_utf8(fmt, (int)STRLEN(fmt), 1, &info, NULL);
    MZ_GC_CHECK();
    byte_string = scheme_make_byte_string(c_string);
    MZ_GC_CHECK();
    argv[0] = scheme_byte_string_to_char_string(byte_string);
    SCHEME_SET_IMMUTABLE(argv[0]);
    MZ_GC_UNREG();
  } else {
    byte_string = scheme_make_byte_string("Emacs error");
    MZ_GC_CHECK();
    argv[0] = scheme_byte_string_to_char_string(byte_string);
    MZ_GC_CHECK();
  }
  MZ_GC_CHECK();

#if MZSCHEME_VERSION_MAJOR < 360
  argv[1] = scheme_current_continuation_marks();
  MZ_GC_CHECK();
#else
  argv[1] = scheme_current_continuation_marks(NULL);
  MZ_GC_CHECK();
#endif

  exn = scheme_make_struct_instance(emacs_exn, 2, argv);
  MZ_GC_CHECK();
  scheme_raise(exn);
  MZ_GC_UNREG();
}


/*
 *========================================================================
 * Exception handling code -- cribbed form the MzScheme sources and
 * Matthew Flatt's "Inside PLT MzScheme" document.
 *========================================================================
 */
static void init_exn_catching_apply() {
  if (!exn_catching_apply) {
    char *e =
	    "(lambda (thunk) "
              "(with-handlers ([void (lambda (exn) (cons #f exn))]) "
                 "(cons #t (thunk))))";
    exn_catching_apply = scheme_eval_string(e, environment);
    MZ_GC_CHECK();
    exn_p = scheme_builtin_value("exn?");
    MZ_GC_CHECK();
    exn_message = scheme_builtin_value("exn-message");
    MZ_GC_CHECK();
  }
}

/*
 * This function applies a thunk, returning the Scheme value if there's
 * no exception, otherwise returning NULL and setting *exn to the raised
 * value (usually an exn structure).
 */
static Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f, Scheme_Object **exn) {
  Scheme_Object *v;

  v = _scheme_apply(exn_catching_apply, 1, &f);
  /* v is a pair: (cons #t value) or (cons #f exn) */

  if (SCHEME_TRUEP(SCHEME_CAR(v))) {
    return SCHEME_CDR(v);
  } else {
    *exn = SCHEME_CDR(v);
    return NULL;
  }
}

static Scheme_Object* extract_exn_message(Scheme_Object *v) {
  if (SCHEME_TRUEP(_scheme_apply(exn_p, 1, &v))) {
    return _scheme_apply(exn_message, 1, &v);
  } else {
    return NULL; /* Not an exn structure */
  }
}
