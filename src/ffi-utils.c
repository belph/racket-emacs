#include <re-config.h>
#include <scheme.h>
#include <emacs-module.h>

#include "racket-rt.h"
#include "racket-rtutils.h"
#include "emacs-c-utils.h"
#include "ffi-utils.h"

emacs_value Fcall_racket_func_raw(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  Scheme_Object *sch_obj = NULL;
  Scheme_Object *args[argc - 1];
  Scheme_Object *value = NULL;
  Scheme_Object *exn = NULL;
  Scheme_Config *oldconfig = NULL;
  Scheme_Config *config = NULL;
  int idx;
  for (idx = 0; idx < argc - 1; ++idx) {
    args[idx] = NULL;
  }
  MZ_GC_DECL_REG(9);
  MZ_GC_ARRAY_VAR_IN_REG(0, args, argc - 1);
  MZ_GC_VAR_IN_REG(3, value);
  MZ_GC_VAR_IN_REG(4, exn);
  MZ_GC_VAR_IN_REG(5, oldconfig);
  MZ_GC_VAR_IN_REG(6, config);
  MZ_GC_VAR_IN_REG(7, sch_obj);
  MZ_GC_REG();
  sch_obj = *((Scheme_Object**)env->get_user_ptr(env, argv[0]));
  for (idx = 0; idx < argc - 1; ++idx) {
    emacs_value arg = argv[idx + 1];
    //fprintf(stderr, "emacs_value arg: %p\n", arg);
    args[idx] = wrap_emacs_value(env, arg, false);
    EMACS_CHECK_EXIT_UNREG(env, NULL);
  }
  MZ_GC_CHECK();
  oldconfig = scheme_current_config();
  config = config_with_env(oldconfig, env);
  scheme_install_config(config);
  value = _apply_func_catch_exceptions(sch_obj, argc - 1, args, &exn);
  scheme_install_config(oldconfig);
  // Null out so non-escaping wrapped values are GC'd
  for (idx = 0; idx < argc - 1; ++idx) {
    args[idx] = NULL;
  }
  MZ_GC_CHECK();

  if (!value) {
    value = extract_exn_message(exn);
    emacs_value Qthrow_tag = env->intern(env, "racket-emacs");
    emacs_value Qthrow_value;
    if (value) {
      Qthrow_value = conv_scheme_string_to_emacs_string(env, value);
      MZ_GC_CHECK();
    } else {
      Qthrow_value = env->intern(env, "nil");
    }
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
      env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
    }
    MZ_GC_UNREG();
  }
  MZ_GC_UNREG();
  return wrap_racket_value(env, value);
}

// min arity: 1, max arity: 1
emacs_value Fscheme_car(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  // Scheme object is from Emacs-land, so it's not GC-able
  if (argc != 1) {
    emacs_value Qthrow_tag = env->intern(env, "racket-emacs");
    emacs_value Qthrow_value = EMACS_STRING(env, "Invalid number of arguments for scheme_car");
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
      env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
    }
    return NULL;
  }
  Scheme_Object *func = NULL;
  Scheme_Object *args[argc];
  Scheme_Object *value = NULL;
  Scheme_Object *exn = NULL;
  Scheme_Config *oldconfig = NULL;
  Scheme_Config *config = NULL;
  int idx;
  for (idx = 0; idx < argc; ++idx) {
    args[idx] = NULL;
  }
  MZ_GC_DECL_REG(9);
  MZ_GC_ARRAY_VAR_IN_REG(0, args, argc);
  MZ_GC_VAR_IN_REG(3, func);
  MZ_GC_VAR_IN_REG(4, value);
  MZ_GC_VAR_IN_REG(5, exn);
  MZ_GC_VAR_IN_REG(6, oldconfig);
  MZ_GC_VAR_IN_REG(7, config);
  MZ_GC_REG();
  for (idx = 0; idx < argc - 1; ++idx) {
    emacs_value arg = argv[idx + 1];
    args[idx] = wrap_emacs_value(env, arg, false);
    EMACS_CHECK_EXIT_UNREG(env, NULL);
  }
  MZ_GC_CHECK();
  oldconfig = scheme_current_config();
  config = config_with_env(oldconfig, env);
  scheme_install_config(config);
  /*mz_jmp_buf * volatile save, fresh;
  th = scheme_get_current_thread();
  save = th->error_buf;
  th->error_buf = &fresh;
  if (scheme_setjmp(*th->error_buf)) {
    // error condition
    th->error_buf;
    emacs_value Qthrow_tag = env->intern(env, "racket-emacs");
    emacs_value Qthrow_value = EMACS_STRING(env, "Error running scheme_car");
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
      env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
    }
    return NULL;
  } else {
    
  }*/
  func = scheme_builtin_value("car");
  value = _apply_func_catch_exceptions(func, argc, args, &exn);
  scheme_install_config(oldconfig);
  // Null out so non-escaping wrapped values are GC'd
  for (idx = 0; idx < argc - 1; ++idx) {
    args[idx] = NULL;
  }
  MZ_GC_CHECK();

  if (!value) {
    value = extract_exn_message(exn);
    emacs_value Qthrow_tag = env->intern(env, "racket-emacs");
    emacs_value Qthrow_value;
    if (value) {
      Qthrow_value = conv_scheme_string_to_emacs_string(env, value);
      MZ_GC_CHECK();
    } else {
      Qthrow_value = env->intern(env, "nil");
    }
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
      env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
    }
    MZ_GC_UNREG();
  }
  MZ_GC_UNREG();
  return wrap_racket_value(env, value);
}

emacs_value Fscheme_symbol(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  Scheme_Object *obj;
  emacs_value ret;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, obj);
  MZ_GC_REG();
  obj = conv_emacs_symbol_to_scheme_symbol(env, argv[0]);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  ret = wrap_racket_value(env, obj);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  MZ_GC_UNREG();
  return ret;
}

void register_ffi_utils_emacs_functions(emacs_env *env) {
  emacs_value fun, val;
  Scheme_Object *listelts[1];
  Scheme_Object *obj;
  MZ_GC_DECL_REG(4);
  MZ_GC_ARRAY_VAR_IN_REG(0, listelts, 1);
  MZ_GC_VAR_IN_REG(3, obj);
  MZ_GC_REG();
  fun = env->make_function(env, 1, -2, Fcall_racket_func_raw, "Invoke the given Racket function", NULL);
  bind_function(env, "call-racket-func-raw", fun);
  
  fun = env->make_function(env, 1, 1, Fscheme_car, "Get the first element of the given pair", NULL);
  bind_function(env, "racket-car", fun);

  fun = env->make_function(env, 1, 1, Fscheme_symbol, "Convert the given symbol to a racket symbol", NULL);
  bind_function(env, "racket-symbol", fun);

  val = env->intern(env, "firstval");
  //fprintf(stderr, "firstval non-global: %p\n", val);
  //val = env->make_global_ref(env, val);
  //fprintf(stderr, "firstval global: %p\n", val);
  listelts[0] = wrap_emacs_value(env, val, false);
  obj = scheme_build_list(1, listelts);
  val = wrap_racket_value(env, obj);
  //fprintf(stderr, "racket-tst-list non-global: %p\n", val);
  //val = env->make_global_ref(env, val);
  //fprintf(stderr, "racket-tst-list global: %p\n", val);
  bind_value(env, "racket-tst-list", val);

  obj = scheme_builtin_value("car");
  val = wrap_racket_value(env, obj);
  //fprintf(stderr, "car non-global: %p\n", val);
  //val = env->make_global_ref(env, val);
  //fprintf(stderr, "car global: %p\n", val);
  bind_value(env, "racket-car", val);
  MZ_GC_UNREG();
}
