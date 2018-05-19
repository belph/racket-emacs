#include <re-config.h>
#include <scheme.h>
#include <emacs-module.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include "racket-rt.h"
#include "racket-rtutils.h"
#include "emacs-c-utils.h"

#define STRLEN(x) ((sizeof(x) / sizeof(char)) - 1)
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

static void wrapped_racket_value_finalizer(void *ptr) {
  //fprintf(stderr, "Running racket_value finalizer for: %p\n", ptr);
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
    //fprintf(stderr, "Unwrapping wrapped emacs value\n");
    emacs_value res = SCHEME_EMACSVALUE_DEREF(value);
    MZ_GC_UNREG();
    return res;
  }
  boxed = scheme_malloc_immobile_box(value);
  //SCHEME_PRINT_STR("Wrapping Racket value: ");
  //SCHEME_DISPLAY(*boxed);
  //fprintf(stderr, " (ptr: %p [box: %p])", *boxed, boxed);

  emacs_value ret = env->make_user_ptr(env, wrapped_racket_value_finalizer, boxed);
  //fflush(stderr);
  //SCHEME_DISPLAY(*((Scheme_Object**)env->get_user_ptr(env, ret)));
  //fprintf(stderr, "\n");
  //SCHEME_PRINT_STR("\n");

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
  //fprintf(stderr, "Running emacs_value finalizer");
  if (SCHEME_EMACSVALUEP(p)) {
    fprintf(stderr, "...(was emacs_value)");
    emacs_env *env = get_current_emacs_env();
    env->free_global_ref(env, SCHEME_EMACSVALUE_DEREF(p));
  }
  fprintf(stderr, "\n");
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
    //fprintf(stderr, "Unwrapping wrapped Racket value (emacs: %p; racket: %p): ", value, ret);
    if (rethrow) {
      RETHROW_EMACS_ERROR(env);
    }
    //fflush(stderr);
    //SCHEME_DISPLAY(ret);
    //fprintf(stderr, "\n");
    MZ_GC_UNREG();
    return ret;
  }
  //fprintf(stderr, "Wrapping Emacs value\n");
  self = scheme_malloc_fail_ok(scheme_malloc_tagged, sizeof(emacs_mz_value));
  memset(self, 0, sizeof(emacs_mz_value));
  MZ_GC_CHECK();
  self->value = env->make_global_ref(env, value);
  self->so.type = mz_emacs_type;
  scheme_register_finalizer(self, wrapped_emacs_value_finalizer, NULL, NULL, NULL);
  //SCHEME_PRINT_STR("Wrapped: ");
  //SCHEME_DISPLAY((Scheme_Object*)self);
  //SCHEME_PRINT_STR("\n");
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
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, config);
  MZ_GC_VAR_IN_REG(1, wrapped);
  MZ_GC_REG();
  wrapped = wrap_emacs_env(env);
  scheme_set_param(config, emacs_env_param, wrapped);
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
  //fprintf(stderr, "Calling: raise_emacs_exn(\"%s\")\n", add_info);
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


void init_racket_rtutils() {
  MZ_REGISTER_STATIC(exn_catching_apply);
  MZ_REGISTER_STATIC(emacs_exn);
  MZ_REGISTER_STATIC(exn_p);
  MZ_REGISTER_STATIC(exn_message);
  MZ_GC_CHECK();
  emacs_env_param = scheme_new_param();
  register_emacs_exn();
  init_exn_catching_apply();
}
