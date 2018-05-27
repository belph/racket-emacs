#include <re-config.h>
#include <scheme.h>
#include <emacs-module.h>

#include "racket-rt.h"
#include "racket-rtutils.h"
#include "emacs-c-utils.h"
#include "ffi-utils.h"
#include "hooks.h"

emacs_value Fcall_racket_func_raw(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  fprintf(stderr, "starting Fcall_racket_func_raw\n");
  Scheme_Object *sch_obj = NULL;
  Scheme_Object *args[argc - 1];
  Scheme_Object *value = NULL;
  Scheme_Object *exn = NULL;
  Scheme_Config *oldconfig = NULL;
  Scheme_Config *config = NULL;
  Scheme_Object *curout = NULL;
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
  MZ_GC_VAR_IN_REG(8, curout);
  MZ_GC_REG();
  fprintf(stderr, "pre-get_user_ptr (argv[0]: %p)\n", argv[0]);
  EMACS_MESSAGE(env, "Fcall_racket_func_raw function: %S", argv[0]);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  Scheme_Object **obj_ref = (Scheme_Object**)env->get_user_ptr(env, argv[0]);
  fprintf(stderr, "pre-deref (obj_ref: %p)\n", obj_ref);
  //sch_obj = *((Scheme_Object**)env->get_user_ptr(env, argv[0]));
  sch_obj = *obj_ref;
  fprintf(stderr, "pre-wrap\n");
  for (idx = 0; idx < argc - 1; ++idx) {
    emacs_value arg = argv[idx + 1];
    //fprintf(stderr, "emacs_value arg: %p\n", arg);
    args[idx] = wrap_emacs_value(env, arg, false);
    EMACS_CHECK_EXIT_UNREG(env, NULL);
  }
  fprintf(stderr, "finished wrap\n");
  MZ_GC_CHECK();
  oldconfig = scheme_current_config();
  config = config_with_env(oldconfig, env);
  scheme_install_config(config);
  fprintf(stderr, "calling: ");
  SCHEME_DISPLAY(sch_obj);
  SCHEME_PRINT_STR("\n");
  value = _apply_func_catch_exceptions(sch_obj, argc - 1, args, &exn);
  scheme_install_config(oldconfig);
  fprintf(stderr, "finished calling\n");
  SCHEME_PRINT_STR("Racket value:");
  SCHEME_DISPLAY(value);
  SCHEME_PRINT_STR("\n");
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
  if (argc != 1) {
    EMACS_EXN(env, "racket-emacs", "Invalid number of arguments for scheme_car");
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

emacs_value Fracket_emacs_wrap_symbol(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
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

static emacs_value conversion_wrapper_helper(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  Scheme_Object *(*conv)(emacs_env *, emacs_value) = data;
  Scheme_Object *obj = NULL;
  emacs_value ret;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, obj);
  MZ_GC_REG();
  obj = conv(env, argv[0]);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  ret = wrap_racket_value(env, obj);
  MZ_GC_UNREG();
  return ret;
}

static emacs_value conversion_unwrapper_helper(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  emacs_value (*conv)(emacs_env *, Scheme_Object *) = data;
  Scheme_Object *unwrapped = NULL;
  emacs_value ret;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, unwrapped);
  MZ_GC_REG();
  unwrapped = wrap_emacs_value(env, argv[0], false);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  ret = conv(env, unwrapped);
  MZ_GC_UNREG();
  return ret;
}

static emacs_value wrapped_racket_build_list(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  Scheme_Object *args[argc];
  Scheme_Object *res = NULL;
  emacs_value ret;
  int idx;
  for (idx = 0; idx < argc; ++idx) {
    args[idx] = NULL;
  }
  MZ_GC_DECL_REG(4);
  MZ_GC_ARRAY_VAR_IN_REG(0, args, argc);
  MZ_GC_VAR_IN_REG(3, res);
  MZ_GC_REG();
  for (idx = 0; idx < argc; ++idx) {
    args[idx] = wrap_emacs_value(env, argv[idx], 0);
    EMACS_CHECK_EXIT_UNREG(env, NULL);
  }
  res = scheme_build_list(argc, args);
  ret = wrap_racket_value(env, res);
  MZ_GC_UNREG();
  return ret;
}

static emacs_value Fset_racket_current_directory(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  Scheme_Object *dir = NULL;
  Scheme_Config *config = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, dir);
  MZ_GC_VAR_IN_REG(1, config);
  MZ_GC_REG();
  dir = conv_emacs_string_to_scheme_string(env, argv[0]);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  config = scheme_current_config();
  scheme_set_param(config, MZCONFIG_CURRENT_DIRECTORY, dir);
  MZ_GC_UNREG();
  return env->intern(env, "nil");
}

static emacs_value Fexpand_requires(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data) {
  Scheme_Object *spec = NULL;
  Scheme_Object *expanded = NULL;
  emacs_value ret;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, spec);
  MZ_GC_VAR_IN_REG(1, expanded);
  MZ_GC_REG();
  spec = wrap_emacs_value(env, argv[0], false);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  expanded = expand_requires(env, spec);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  ret = wrap_racket_value(env, expanded);
  MZ_GC_UNREG();
  return ret;
}

// For macros below
#ifdef bool
#undef bool
#endif

#define BIND_WRAPPER(env, type_name, Ftype_name, type_doc)       \
  EMACS_BIND_FUNCTION(env, "racket-emacs/wrap-" type_name,       \
                      conversion_wrapper_helper,                 \
                      1, 1,                                        \
                      "Wrap the given Emacs " type_doc,              \
                      conv_emacs_ ## Ftype_name ## _to_scheme_ ## Ftype_name)

#define BIND_WRAPPER_SIMPLE(env, type) BIND_WRAPPER(env, #type, type, #type)

#define BIND_UNWRAPPER(env, type_name, Ftype_name, type_doc)       \
  EMACS_BIND_FUNCTION(env, "racket-emacs/unwrap-" type_name,       \
                      conversion_unwrapper_helper,                 \
                      1, 1,                                        \
                      "Unwrap the given Racket " type_doc,              \
                      conv_scheme_ ## Ftype_name ## _to_emacs_ ## Ftype_name)

#define BIND_UNWRAPPER_SIMPLE(env, type) BIND_UNWRAPPER(env, #type, type, #type)

#define BIND_WRAPPER_FUNCS(env, type_name, Ftype_name, type_doc) \
  BIND_WRAPPER(env, type_name, Ftype_name, type_doc); \
  BIND_UNWRAPPER(env, type_name, Ftype_name, type_doc)

#define BIND_WRAPPER_FUNCS_SIMPLE(env, type) \
  BIND_WRAPPER_SIMPLE(env, type); \
  BIND_UNWRAPPER_SIMPLE(env, type)

#define BIND_SCHEME_VAL(env, name, value) \
  do {\
    emacs_env *__env_sch = env;                 \
    Scheme_Object *__sch_bind_val = NULL;       \
    MZ_GC_DECL_REG(1);                          \
    MZ_GC_VAR_IN_REG(0, __sch_bind_val);        \
    MZ_GC_REG();                                \
    __sch_bind_val = value;                     \
    bind_value(__env_sch, "racket-emacs/" name, wrap_racket_value(__env_sch, __sch_bind_val)); \
    MZ_GC_UNREG();                              \
  } while (0)

#define BIND_SCHEME_VAL_SIMPLE(env, type) \
  BIND_SCHEME_VAL(env, #type, scheme_ ## type)

static int register_ffi_utils_emacs_functions(emacs_env *env) {
  emacs_value val;
  Scheme_Object *listelts[1];
  Scheme_Object *obj;
  MZ_GC_DECL_REG(4);
  MZ_GC_ARRAY_VAR_IN_REG(0, listelts, 1);
  MZ_GC_VAR_IN_REG(3, obj);
  MZ_GC_REG();

  EMACS_BIND_FUNCTION(env, "racket-emacs/runtime/call-raw",
                      Fcall_racket_func_raw,
                      1, EMACS_ARGS_MANY,
                      "Invoke the given Racket function",
                      NULL);

  EMACS_BIND_FUNCTION(env, "racket-emacs/runtime/set-current-directory",
                      Fset_racket_current_directory,
                      1, 1,
                      "Set the (current-directory) Racket parameter",
                      NULL);

  EMACS_BIND_FUNCTION(env, "racket-emacs/runtime/raw/expand-requires",
                      Fexpand_requires,
                      1, 1,
                      "Expand the given (quoted) require spec",
                      NULL);

  EMACS_BIND_FUNCTION(env, "racket-emacs--unwrap-primitive",
                      conversion_unwrapper_helper,
                      1, 1,
                      "Wrap the given Emacs primitive value, as possible",
                      conv_scheme_primitive_to_emacs_primitive);

  EMACS_BIND_FUNCTION(env, "racket-emacs/build-list",
                      wrapped_racket_build_list,
                      1, EMACS_ARGS_MANY,
                      "Build a Racket list from the given arguments",
                      NULL);

  BIND_WRAPPER_FUNCS_SIMPLE(env, integer);
  BIND_WRAPPER_FUNCS_SIMPLE(env, float);
  BIND_WRAPPER_FUNCS_SIMPLE(env, symbol);
  BIND_WRAPPER_FUNCS_SIMPLE(env, string);
  BIND_WRAPPER_FUNCS_SIMPLE(env, bool);
  BIND_WRAPPER_FUNCS(env, "cons", pair, "cons cell");

  BIND_SCHEME_VAL_SIMPLE(env, eof);
  BIND_SCHEME_VAL_SIMPLE(env, null);
  BIND_SCHEME_VAL_SIMPLE(env, true);
  BIND_SCHEME_VAL_SIMPLE(env, false);
  BIND_SCHEME_VAL_SIMPLE(env, void);

  BIND_SCHEME_VAL(env, "raw/car", scheme_builtin_value("car"));
  BIND_SCHEME_VAL(env, "raw/dynamic-require", scheme_builtin_value("dynamic-require"));
  
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

  MZ_GC_UNREG();
  return 0;
}

RE_SETUP_HOOK("ffi-utils-init", POST_RACKETUTILS_INIT, register_ffi_utils_emacs_functions)
