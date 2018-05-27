#include <re-config.h>
#include <re-macros.h>
#include <scheme.h>
#include <emacs-module.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

#include "racket-rt.h"
#include "racket-rtutils.h"
#include "emacs-c-utils.h"
#include "hooks.h"

typedef struct {
  Scheme_Config *old_config;
  emacs_env *cur_env;
  Scheme_Object* (*worker_fun)(void*);
  Scheme_Object* (*jump_handler)(void*);
  void* data;
} emacs_mz_env_ctxt;

static int emacs_env_param;
static Scheme_Object *exn_catching_apply = NULL;
static Scheme_Object *emacs_exn = NULL;
static Scheme_Object *exn_p = NULL;
static Scheme_Object *exn_message = NULL;
static Scheme_Object *do_resolve_requires = NULL;
static Scheme_Env *resolve_require_namespace = NULL;

static void wrapped_racket_value_finalizer(void *ptr) {
  dprintf("Running racket_value finalizer for: %p\n", ptr);
  scheme_free_immobile_box(ptr);
}

emacs_value wrap_racket_value(emacs_env *env, Scheme_Object *value) {
  void **boxed = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, value);
  MZ_GC_VAR_IN_REG(1, boxed);
  MZ_GC_REG();

  // If this is already a wrapped emacs value, just unwrap it.
  if (SCHEME_EMACSVALUEP(value)) {
    dprintf("Unwrapping wrapped emacs value\n");
    emacs_value res = SCHEME_EMACSVALUE_DEREF(value);
    MZ_GC_UNREG();
    return res;
  }
  boxed = scheme_malloc_immobile_box(value);
  SCHEME_PRINT_STR("Wrapping Racket value: ");
  SCHEME_DISPLAY(*boxed);
  dprintf(" (ptr: %p [box: %p]) ", *boxed, boxed);

  emacs_value ret = env->make_user_ptr(env, wrapped_racket_value_finalizer, boxed);
  SCHEME_DISPLAYLN(*((Scheme_Object**)env->get_user_ptr(env, ret)));

  MZ_GC_UNREG();
  return ret;
}

Scheme_Object *wrap_emacs_env(emacs_env *env) {
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

static void wrapped_emacs_value_finalizer(void *p, void *data) {
  dprintf("Running emacs_value finalizer");
  if (SCHEME_EMACSVALUEP(p)) {
    dprintf("...(was emacs_value)");
    emacs_env *env = get_current_emacs_env();
    env->free_global_ref(env, SCHEME_EMACSVALUE_DEREF(p));
  }
  dprintf("\n");
}

Scheme_Object *wrap_emacs_value(emacs_env *env, emacs_value value, int rethrow) {
  emacs_mz_value *self = NULL;
  Scheme_Object *ret = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, self);
  MZ_GC_VAR_IN_REG(1, ret);
  MZ_GC_REG();
  if (emacs_is_user_ptr(env, value)) {
    // FIXME: We shouldn't assume every user pointer is a wrapped Scheme_Object*
    ret = *((Scheme_Object**)env->get_user_ptr(env, value));
    dprintf("Unwrapping wrapped Racket value (emacs: %p; racket: %p): ", value, ret);
    if (rethrow) {
      RETHROW_EMACS_ERROR(env);
    }
    SCHEME_DISPLAYLN(ret);
    MZ_GC_UNREG();
    return ret;
  }
  dprintf("Wrapping Emacs value\n");
  self = scheme_malloc_fail_ok(scheme_malloc_tagged, sizeof(emacs_mz_value));
  memset(self, 0, sizeof(emacs_mz_value));
  MZ_GC_CHECK();
  self->value = env->make_global_ref(env, value);
  self->so.type = mz_emacs_type;
  scheme_register_finalizer(self, wrapped_emacs_value_finalizer, NULL, NULL, NULL);
  SCHEME_PRINT_STR("Wrapped: ");
  SCHEME_DISPLAYLN((Scheme_Object*)self);
  MZ_GC_UNREG();
  return (Scheme_Object*)self;
}

Scheme_Object *unwrap_emacs_value(emacs_env *env, emacs_value value, int rethrow) {
  if (!emacs_is_user_ptr(env, value)) {
    EMACS_EXN(env, "racket-emacs", "Not a user pointer!");
    return NULL;
  }
  Scheme_Object *ret;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, ret);
  MZ_GC_REG();
  ret = *((Scheme_Object**)env->get_user_ptr(env, value));
  if (rethrow) {
    RETHROW_EMACS_ERROR(env);
  }
  MZ_GC_UNREG();
  return ret;
}


static void prepare_env(void *envptr) {
  emacs_mz_env_ctxt *ctxt = (emacs_mz_env_ctxt*)envptr;
  Scheme_Config *new_config = NULL;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, new_config);
  MZ_GC_REG();
  ctxt->old_config = scheme_current_config();
  MZ_GC_CHECK();
  new_config = config_with_env(ctxt->old_config, ctxt->cur_env);
  scheme_install_config(new_config);
  MZ_GC_UNREG();
}

static void teardown_env(void *envptr) {
  emacs_mz_env_ctxt *ctxt = (emacs_mz_env_ctxt*)envptr;
  scheme_install_config(ctxt->old_config);
}

static Scheme_Object *run_worker(void *envptr) {
  emacs_mz_env_ctxt *ctxt = (emacs_mz_env_ctxt*)envptr;
  return ctxt->worker_fun(ctxt->data);
}

static Scheme_Object *run_jump_handler(void *envptr) {
  emacs_mz_env_ctxt *ctxt = (emacs_mz_env_ctxt*)envptr;
  return ctxt->jump_handler(ctxt->data);
}

Scheme_Object *with_env(emacs_env *env, Scheme_Object* (*worker_fun)(void*), Scheme_Object* (*jump_handler)(void*), void *data) {
  emacs_mz_env_ctxt ctxt;
  ctxt.old_config = NULL;
  Scheme_Object *ret = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, ctxt.old_config);
  MZ_GC_VAR_IN_REG(1, ret);
  MZ_GC_REG();
  ctxt.cur_env = env;
  ctxt.worker_fun = worker_fun;
  ctxt.jump_handler = jump_handler;
  ctxt.data = data;
  ret = scheme_dynamic_wind(prepare_env, run_worker, teardown_env, run_jump_handler, &ctxt);
  MZ_GC_UNREG();
  return ret;
}


emacs_env *get_emacs_env(Scheme_Config *config) {
  Scheme_Object *obj = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, config);
  MZ_GC_VAR_IN_REG(1, obj);
  MZ_GC_REG();
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

emacs_env *get_current_emacs_env() {
  Scheme_Config *config = NULL;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, config);
  MZ_GC_REG();
  config = scheme_current_config();
  emacs_env *ret = get_emacs_env(config);
  MZ_GC_UNREG();
  return ret;
}

void set_emacs_env(Scheme_Config *config, emacs_env *env) {
  Scheme_Object *wrapped = NULL;
  Scheme_Object *dir = NULL;
  MZ_GC_DECL_REG(3);
  MZ_GC_VAR_IN_REG(0, config);
  MZ_GC_VAR_IN_REG(1, wrapped);
  MZ_GC_VAR_IN_REG(2, dir);
  MZ_GC_REG();
  wrapped = wrap_emacs_env(env);
  scheme_set_param(config, emacs_env_param, wrapped);
  emacs_value edir = emacs_load_file_directory(env);
  EMACS_CHECK_EXIT_UNREG(env,);
  dir = conv_emacs_string_to_scheme_string(env, edir);
  scheme_set_param(config, MZCONFIG_CURRENT_DIRECTORY, dir);
  MZ_GC_UNREG();
}

void set_current_emacs_env(emacs_env *env) {
  Scheme_Config *config = NULL;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, config);
  MZ_GC_REG();
  config = scheme_current_config();
  set_emacs_env(config, env);
  MZ_GC_UNREG();
}

Scheme_Config *config_with_env(Scheme_Config *config, emacs_env *env) {
  return scheme_extend_config(config, emacs_env_param, wrap_emacs_env(env));
}

Scheme_Config *current_config_with_env(emacs_env *env) {
  Scheme_Config *curconfig = NULL;
  Scheme_Config *ret = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, curconfig);
  MZ_GC_VAR_IN_REG(1, ret);
  MZ_GC_REG();
  curconfig = scheme_current_config();
  ret = config_with_env(curconfig, env);
  MZ_GC_UNREG();
  return ret;
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

    for (i = 0; i < nc; i++) {
      scheme_add_global_symbol(exn_names[i],
                               exn_values[i], get_racket_env());
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
  dprintf("Calling: raise_emacs_exn(\"%s\")\n", add_info);
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
	    "(lambda (func args) "
              "(with-handlers ([void (lambda (exn) (cons #f exn))]) "
                 "(cons #t (apply func args))))";
    exn_catching_apply = scheme_eval_string(e, get_racket_env());
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
Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f, Scheme_Object **exn) {
  Scheme_Object *v;
  Scheme_Object *arglist = NULL;
  Scheme_Object *args[2] = {NULL, NULL};
  MZ_GC_DECL_REG(4);
  MZ_GC_VAR_IN_REG(0, arglist);
  MZ_GC_ARRAY_VAR_IN_REG(1, args, 2);
  MZ_GC_REG();
  arglist = scheme_null;
  args[0] = f;
  args[1] = arglist;

  v = _scheme_apply(exn_catching_apply, 2, args);
  /* v is a pair: (cons #t value) or (cons #f exn) */

  MZ_GC_UNREG();
  if (SCHEME_TRUEP(SCHEME_CAR(v))) {
    return SCHEME_CDR(v);
  } else {
    *exn = SCHEME_CDR(v);
    return NULL;
  }
}

Scheme_Object *_apply_func_catch_exceptions(Scheme_Object *f, int argc, Scheme_Object *argv[], Scheme_Object **exn) {
  Scheme_Object *v;
  Scheme_Object *arglist = NULL;
  Scheme_Object *args[2] = {NULL, NULL};
  Scheme_Config *config = NULL;
  Scheme_Object *curout = NULL;
  //GC_dump();
  MZ_GC_DECL_REG(6);
  MZ_GC_VAR_IN_REG(0, arglist);
  MZ_GC_ARRAY_VAR_IN_REG(1, args, 2);
  MZ_GC_VAR_IN_REG(4, config);
  MZ_GC_VAR_IN_REG(5, curout);
  MZ_GC_REG();
  args[0] = f;
  args[1] = scheme_build_list(argc, argv);
  //config = scheme_current_config();
  //curout = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);
  //scheme_display(f, curout);
  //scheme_display(scheme_make_char('\n'), curout);
  //scheme_display(args[1], curout);
  //scheme_display(scheme_make_char('\n'), curout);
  //scheme_flush_output(curout);
  
  v = _scheme_apply(exn_catching_apply, 2, args);
  /* v is a pair: (cons #t value) or (cons #f exn) */

  MZ_GC_UNREG();
  if (SCHEME_TRUEP(SCHEME_CAR(v))) {
    return SCHEME_CDR(v);
  } else {
    *exn = SCHEME_CDR(v);
    return NULL;
  }
}

Scheme_Object* extract_exn_message(Scheme_Object *v) {
  if (SCHEME_TRUEP(_scheme_apply(exn_p, 1, &v))) {
    return _scheme_apply(exn_message, 1, &v);
  } else {
    return NULL; /* Not an exn structure */
  }
}

static Scheme_Object *closed_dynamic_require(int argc, Scheme_Object **argv, Scheme_Object *prim) {
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

  MZ_GC_CHECK();

  ret = scheme_dynamic_require(2, a);
  MZ_GC_UNREG();
  return ret;
}

static Scheme_Object *racket_do_safe_run_prim(void *data) {
  emacs_mz_prim_runinfo *runinfo = data;
  Scheme_Object   *value = NULL;
  Scheme_Object   *exn = NULL;
  Scheme_Object   *prim = NULL;
  MZ_GC_DECL_REG(6);
  MZ_GC_VAR_IN_REG(0, value);
  MZ_GC_VAR_IN_REG(1, exn);
  MZ_GC_VAR_IN_REG(2, prim);
  MZ_GC_ARRAY_VAR_IN_REG(3, runinfo->primv, runinfo->primc);
  MZ_GC_REG();
  
  prim = scheme_make_prim_closure_w_arity(runinfo->proc, runinfo->primc, runinfo->primv,
                                          runinfo->proc_name, 0, 0);
  MZ_GC_CHECK();
  //dprintf("Pre-apply thunk\n");
  value = _apply_thunk_catch_exceptions(prim, &exn);
  //dprintf("Post-apply thunk\n");
  MZ_GC_CHECK();

  if (!value) {
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
  MZ_GC_UNREG();
  return value;
}

static Scheme_Object *impossible(void *ptr) {
  fprintf(stderr, "racket-emacs Fatal internal error: impossible() reached!\n");
  fflush(stderr);
  abort();
  return NULL;
}

Scheme_Object *racket_safe_run_prim(emacs_mz_prim_runinfo *runinfo) {
  if (runinfo->env) {
    return with_env(runinfo->env, racket_do_safe_run_prim, impossible, runinfo);
  } else {
    // if env is NULL, don't update
    return racket_do_safe_run_prim(runinfo);
  }
}

Scheme_Object *racket_safe_dynamic_require(emacs_env *env, Scheme_Object *mod, Scheme_Object *val) {
  emacs_mz_prim_runinfo runinfo;
  Scheme_Object *a[2] = { NULL, NULL };
  Scheme_Object *ret = NULL;
  MZ_GC_DECL_REG(4);
  MZ_GC_ARRAY_VAR_IN_REG(0, a, 2);
  MZ_GC_VAR_IN_REG(3, ret);
  MZ_GC_REG();
  a[0] = mod;
  a[1] = (val == NULL) ? scheme_false : val;
  runinfo.primv = a;
  runinfo.primc = 2;
  runinfo.env = env;
  runinfo.proc = closed_dynamic_require;
  runinfo.proc_name = "closed-dynamic-require";

  ret = racket_safe_run_prim(&runinfo);
  MZ_GC_CHECK();
  MZ_GC_UNREG();
  return ret;
}

static void setup_resolve_requires(emacs_env *env) {
  Scheme_Object *mod = NULL;
  Scheme_Object *val = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, mod);
  MZ_GC_VAR_IN_REG(1, val);
  MZ_GC_REG();

  mod = scheme_intern_symbol("racket-emacs/private/resolve-require");
  val = scheme_intern_symbol("do-resolve-requires");
  do_resolve_requires = racket_safe_dynamic_require(env, mod, val);
  MZ_GC_UNREG();
}

static Scheme_Object* do_expand_requires(int argc, Scheme_Object **argv, Scheme_Object *prim) {
  Scheme_Object *spec = NULL;
  Scheme_Object *sym = NULL;
  Scheme_Object *ret = NULL;
  MZ_GC_DECL_REG(6);
  MZ_GC_ARRAY_VAR_IN_REG(0, prim, 1);
  MZ_GC_VAR_IN_REG(3, spec);
  MZ_GC_VAR_IN_REG(4, sym);
  MZ_GC_VAR_IN_REG(5, ret);
  MZ_GC_REG();
  spec = SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  sym = scheme_intern_symbol("do-resolve-requires");
  MZ_GC_CHECK();
  ret = scheme_apply(do_resolve_requires, 1, &spec);
  MZ_GC_CHECK();
  MZ_GC_UNREG();
  return ret;
}

Scheme_Object *expand_requires(emacs_env *env, Scheme_Object *v) {
  if (do_resolve_requires == NULL) {
    raise_emacs_exn("racket-rtutils not initialized! (do_resolve_requires was NULL)");
    return NULL; // <- won't be reached
  }
  Scheme_Object *ret = NULL;
  emacs_mz_prim_runinfo runinfo;
  runinfo.primv = &v;
  MZ_GC_DECL_REG(4);
  MZ_GC_ARRAY_VAR_IN_REG(0, runinfo.primv, 1);
  MZ_GC_VAR_IN_REG(3, ret);
  MZ_GC_REG();
  runinfo.primc = 1;
  runinfo.env = env;
  runinfo.proc = do_expand_requires;
  runinfo.proc_name = "do-expand-requires";
  SCHEME_PRINT_STR("expanding requirements: ");
  SCHEME_DISPLAYLN(runinfo.primv[0]);
  ret = racket_safe_run_prim(&runinfo);
  SCHEME_PRINT_STR("expanded requirements: ");
  if (ret) {
    SCHEME_DISPLAYLN(ret);
  } else {
    SCHEME_PRINT_STR("<NULL>\n");
  }
  MZ_GC_UNREG();
  return ret;
}

int init_racket_rtutils(emacs_env *eenv) {
  dprintf("init_racket_rtutils (env: %p)\n", eenv);
  MZ_REGISTER_STATIC(exn_catching_apply);
  MZ_REGISTER_STATIC(emacs_exn);
  MZ_REGISTER_STATIC(exn_p);
  MZ_REGISTER_STATIC(exn_message);
  MZ_REGISTER_STATIC(do_resolve_requires);
  MZ_REGISTER_STATIC(resolve_require_namespace);
  MZ_GC_CHECK();
  emacs_env_param = scheme_new_param();
  register_emacs_exn();
  init_exn_catching_apply();
  setup_resolve_requires(eenv);
  run_hooks(POST_RACKETUTILS_INIT, eenv);
  return 0;
}

RE_SETUP_HOOK("racket-rtutils",
              POST_RACKET_INIT,
              init_racket_rtutils)

