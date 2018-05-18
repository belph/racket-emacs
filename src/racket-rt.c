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
#include "racket-rtutils.h"

// A large portion of this file is taken from VIM's mzscheme (i.e. Racket) module,
// as it addresses many nuances of portability and alternate Racket configurations.

#define STRLEN(x) ((sizeof(x) / sizeof(char)) - 1)

#ifdef RE_DEBUG_GC
#ifdef MZ_PRECISE_GC
#undef MZ_GC_DECL_REG
#define MZ_GC_DECL_REG(size) void *__gc_var_stack__[size+2] = { (void *)0, (void *)size }; fprintf(stderr, "%s __gc_var_stack__: %p\n", __func__, __gc_var_stack__)
#endif // MZ_PRECISE_GC
#endif // RE_DEBUG_GC


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
  Scheme_Object *sym;
  Scheme_Object **res;
} filerun_info;

static bool initialized = false;
static bool load_base_module_failed = false;

/* global environment */
static Scheme_Env    *environment = NULL;
/* output/error handlers */
static Scheme_Object *curout = NULL;
static Scheme_Object *curerr = NULL;

#if !defined(MZ_PRECISE_GC) || MZSCHEME_VERSION_MAJOR < 400
static void *stack_base = NULL;
#endif

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

Scheme_Env *get_racket_env() {
  return environment;
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
  //scheme_set_logging(SCHEME_LOG_FATAL, SCHEME_LOG_DEBUG);

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
  //fprintf(stderr, "Start closed_dynamic_require\n");
  Scheme_Object *a[2] = {NULL, NULL};
  Scheme_Object *ret = NULL;
  Scheme_Object *curout = NULL;
  Scheme_Config *config = NULL;
  MZ_GC_DECL_REG(7);
  MZ_GC_ARRAY_VAR_IN_REG(0, a, 2);
  MZ_GC_VAR_IN_REG(3, ret);
  MZ_GC_VAR_IN_REG(4, curout);
  MZ_GC_VAR_IN_REG(5, config);
  MZ_GC_VAR_IN_REG(6, prim);
  MZ_GC_REG();
  a[0] = SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  a[1] = SCHEME_PRIM_CLOSURE_ELS(prim)[1];
  //fprintf(stderr, "a[0] addr: %p\n", a[0]);
  //fprintf(stderr, "a[1] addr: %p\n", a[1]);

  //config = scheme_current_config();
  //curout = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);
  //fprintf(stderr, "Pre-print 0:\n");
  //scheme_display(a[0], curout);
  //scheme_flush_output(curout);
  //fprintf(stderr, "\nPre-print 1:\n");
  //scheme_display(a[1], curout);
  //scheme_flush_output(curout);
  MZ_GC_CHECK();

  //fprintf(stderr, "\nPre-call\n");
  //fprintf(stderr, "a[0] addr: %p\n", a[0]);
  //fprintf(stderr, "a[1] addr: %p\n", a[1]);
  //fprintf(stderr, "ret: %p\n", ret);
  //fprintf(stderr, "curout: %p\n", curout);
  //fprintf(stderr, "config: %p\n", config);
  ret = scheme_dynamic_require(2, a);
  //fprintf(stderr, "Post-call\n");
  MZ_GC_UNREG();
  //fprintf(stderr, "End closed_dynamic_require\n");
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

  init_racket_rtutils();
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

int racket_init(emacs_env *emacs_env) {
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
    //Scheme_Config	*config = NULL;
    //MZ_GC_DECL_REG(1);
    //MZ_GC_VAR_IN_REG(0, config);
    //MZ_GC_REG();
    //config = scheme_current_config();
    //MZ_GC_CHECK();
    /* recreate ports each call effectively clearing these ones */
    //curout = scheme_make_byte_string_output_port();
    //MZ_GC_CHECK();
    //curerr = scheme_make_byte_string_output_port();
    //MZ_GC_CHECK();
    //scheme_set_param(config, MZCONFIG_OUTPUT_PORT, curout);
    //MZ_GC_CHECK();
    //scheme_set_param(config, MZCONFIG_ERROR_PORT, curerr);
    //MZ_GC_UNREG();
  }

  return 0;
}

static Scheme_Object *do_eval_racket_file(void *frinfo_ptr) {
  filerun_info *frinfo = (filerun_info*)frinfo_ptr;
  char *filename = frinfo->filename;
  Scheme_Object   *value = NULL;
  Scheme_Object   *exn = NULL;
  Scheme_Object   *prim = NULL;
  Scheme_Object *a[2] = {NULL, NULL};
  MZ_GC_DECL_REG(6);
  MZ_GC_VAR_IN_REG(0, value);
  MZ_GC_VAR_IN_REG(1, exn);
  MZ_GC_VAR_IN_REG(2, prim);
  MZ_GC_ARRAY_VAR_IN_REG(3, a, 3);
  MZ_GC_REG();

  a[0] = scheme_make_pair(scheme_intern_symbol("file"),
			  scheme_make_pair(scheme_make_utf8_string(filename),
					   scheme_null));
  a[1] = frinfo->sym;
  
  prim = scheme_make_prim_closure_w_arity(closed_dynamic_require, 2, a, "closed-dynamic-require", 0, 0);
  MZ_GC_CHECK();
  //fprintf(stderr, "Pre-apply thunk\n");
  value = _apply_thunk_catch_exceptions(prim, &exn);
  //fprintf(stderr, "Post-apply thunk\n");
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
  fprintf(stderr, "racket-emacs Fatal internal error: impossible() reached!\n");
  fflush(stderr);
  abort();
  return NULL;
}

emacs_value Feval_racket_file(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  char *filename = emacs_string_to_c_string(env, argv[0]);
  EMACS_CHECK_EXIT(env, NULL);
  //fprintf(stderr, "Pre-init\n");
  //fprintf(stderr, "Post-init\n");
  Scheme_Object *res = NULL;
  filerun_info frinfo = { filename, NULL, &res };
  
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, res);
  MZ_GC_VAR_IN_REG(1, frinfo.sym);
  MZ_GC_REG();
  if (argc > 1) {
    frinfo.sym = conv_emacs_symbol_to_scheme_symbol(env, argv[1]);
  } else {
    frinfo.sym = scheme_make_false();
  }
  with_env(env, do_eval_racket_file, impossible, &frinfo);
  free(filename);
  emacs_value ret = wrap_racket_value(env, *frinfo.res);
  MZ_GC_UNREG();
  return ret;
}

