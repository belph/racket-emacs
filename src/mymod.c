#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <emacs-module.h>
#include <scheme.h>
#include <ctype.h>
//#include "base.c"

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

#define MZ_COLLECTION_PATH "/usr/racket/share/racket/collects"
#define MZ_CONFIG_PATH "/usr/racket/etc/racket/config.rktd"

emacs_env *eenv = NULL;
emacs_value *em_val = NULL;

typedef enum racket_emacs_value_type {
  rev_integer,
  rev_float,
  rev_symbol,
  rev_string,
  rev_vector,
  rev_procedure,
  rev_cons,
  rev_user_ptr
} racket_emacs_value_type;

/*struct racket_emacs_rtinfo_s {
  Scheme_Env *env;
  mz_jmp_buf *volatile error_buf;
} racket_emacs_rtinfo_t;

static racket_emacs_rtinfo_t global_rtinfo = { NULL, NULL };

static void racket_emacs_finalize_scheme_env(void *rtinfoptr) {
  racket_emacs_rtinfo_t *rtinfo = (racket_emacs_rtinfo_t*)rtinfoptr;
  if (rtinfoptr != &global_rtinfo) {
    fprintf(stderr, "Finalized non-global environment\n");
  }
  scheme_current_thread->error_buf = rtinfo->error_buf;
  }*/


static emacs_value scheme_object_to_emacs_value(emacs_env *env, Scheme_Object *value);

// Source: http://phst.github.io/emacs-modules.html#interning
static bool
safe_intern (emacs_env *env, const char *name, size_t size, emacs_value *result)
{
  bool simple = true;
  for (size_t i = 0; i < size; ++i)
    if (name[i] == '\0' || !isascii (name[i]))
      {
        simple = false;
        break;
      }
  if (simple)
    *result = env->intern (env, name);
  else
    {
      emacs_value string_object = env->make_string(env, name, (ptrdiff_t) size);
      *result = env->funcall (env, env->intern (env, "intern"),
                              1, &string_object);
    }
  return env->non_local_exit_check (env) == emacs_funcall_exit_return;
}


static int run(Scheme_Env *e, int argc, char *argv[])
{
  Scheme_Object *curout = NULL, *v = NULL, *v2 = NULL, *a[2] = {NULL, NULL};
  Scheme_Config *config = NULL;
  int i;
  mz_jmp_buf * volatile save = NULL, fresh;
  
  MZ_GC_DECL_REG(8);
  MZ_GC_VAR_IN_REG(0, e);
  MZ_GC_VAR_IN_REG(1, curout);
  MZ_GC_VAR_IN_REG(2, save);
  MZ_GC_VAR_IN_REG(3, config);
  MZ_GC_VAR_IN_REG(4, v);
  MZ_GC_ARRAY_VAR_IN_REG(5, a, 2);
  
  MZ_GC_REG();
  
  //declare_modules(e);
  scheme_set_collects_path(scheme_make_path(MZ_COLLECTION_PATH));
  scheme_set_config_path(scheme_make_path(MZ_CONFIG_PATH));
  scheme_init_collection_paths(e, scheme_null);
  
  v = scheme_intern_symbol("racket/base");
  scheme_namespace_require(v);
  
  config = scheme_current_config();
  curout = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);
  //v = scheme_make_char('\n');
  //scheme_display(v, curout);
  /* read-eval-print loop, uses initial Scheme_Env: */
  a[0] = scheme_make_pair(scheme_intern_symbol("file"),
			  scheme_make_pair(scheme_make_utf8_string("tst.rkt"),
					   scheme_make_null()));
  a[1] = scheme_intern_symbol("val");
  //a[0] = scheme_intern_symbol("racket/base");
  //a[1] = scheme_intern_symbol("current-library-collection-paths");
  v = scheme_dynamic_require(2, a);
  scheme_display(v, curout);
  //scheme_apply(v, 0, NULL);
  //v = scheme_load("/home/pblair/views/racket-emacs/tst.rkt");

  /*if (SCHEME_INTP(v)) {
    scheme_current_thread->error_buf = save;
    MZ_GC_UNREG();
    return SCHEME_INT_VAL(v);
  }
  printf("non-int\n");
  scheme_display(v, curout);*/
  if (em_val != NULL && eenv != NULL) {
    *em_val = scheme_object_to_emacs_value(eenv, v);
  }
  scheme_current_thread->error_buf = save;
  MZ_GC_UNREG();
  fflush(stdout);
  return 0;
}

#if defined(WIN32) && defined(USE_THREAD_LOCAL)
static __declspec(thread) void *tls_space;
#endif

static int
run_racket (int argc, char *argv[]) {
#if defined(WIN32) && defined(USE_THREAD_LOCAL)
  scheme_register_tls_space(&tls_space, 0);
#endif
  return scheme_main_setup(1, run, argc, argv);
}

static char *emacs_string_to_c_string_with_size(emacs_env *env, emacs_value value, ptrdiff_t *size) {
  emacs_value Qthrow_tag, Qthrow_value;
  ptrdiff_t bufsize = 0;
  // Fetch required buffer size
  env->copy_string_contents(env, value, NULL, &bufsize);
  if (bufsize == 0) {
    Qthrow_tag = env->intern(env, "racket-emacs-internal");
    char zero_msg[] = "Internal error: get_emacs_type: Buffer size was zero!";
    Qthrow_value = env->make_string(env, zero_msg, (sizeof(zero_msg) / sizeof(char)) - 1);
    env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
  }
  // Allocate the buffer
  char *buf = malloc(bufsize);
  if (size != NULL) {
    *size = bufsize;
  }
  if (buf == NULL) {
    Qthrow_tag = env->intern(env, "racket-emacs-internal");
    char malloc_msg[] = "Internal error: get_emacs_type: Failed to allocate buffer!";
    Qthrow_value = env->make_string(env, malloc_msg, (sizeof(malloc_msg) / sizeof(char)) - 1);
    env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
  }
  // Copy the string into allocated buffer
  if (!env->copy_string_contents(env, value, buf, &bufsize)) {
    Qthrow_tag = env->intern(env, "racket-emacs-internal");
    char copy_msg[] = "Internal error: get_emacs_type: Failed to copy string contents!";
    Qthrow_value = env->make_string(env, copy_msg, (sizeof(copy_msg) / sizeof(char)) - 1);
    env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
  }
  return buf;
}

static char *emacs_string_to_c_string(emacs_env *env, emacs_value value) {
  return emacs_string_to_c_string_with_size(env, value, NULL);
}

static emacs_value emacs_symbol_name(emacs_env *env, emacs_value Qsym) {
  emacs_value Qsymbol_name = env->intern(env, "symbol-name");
  emacs_value args[] = { Qsym };
  return env->funcall(env, Qsymbol_name, 1, args);
}

static emacs_value scheme_object_to_emacs_value(emacs_env *env, Scheme_Object *value) {
  if (SCHEME_EXACT_INTEGERP(value)) {
    return env->make_integer(env, (intmax_t)SCHEME_INT_VAL(value));
  }
  if (SCHEME_REALP(value)) {
    return env->make_float(env, SCHEME_DBL_VAL(value));
  }
  if (SCHEME_CHAR_STRINGP(value)) {
    value = scheme_char_string_to_byte_string(value);
    char *buf = SCHEME_BYTE_STR_VAL(value);
    ptrdiff_t bytelen = SCHEME_BYTE_STRLEN_VAL(value);
    return env->make_string(env, buf, bytelen);
  }
  if (SCHEME_VECTORP(value)) {
    emacs_value Qvector = env->intern(env, "vector");
    Scheme_Object **sch_elts = SCHEME_VEC_ELS(value);
    size_t vec_size = SCHEME_VEC_SIZE(value);
    emacs_value args[vec_size];
    int i;
    for (i = 0; i < vec_size; ++i) {
      args[i] = scheme_object_to_emacs_value(env, sch_elts[i]);
    }
    return env->funcall(env, Qvector, vec_size, args);
  }
  if (SCHEME_PROCP(value)) {
    return env->intern(env, "procedures-unsupported");
  }
  if (SCHEME_PAIRP(value)) {
    emacs_value Qcons = env->intern(env, "cons");
    emacs_value Qcar = scheme_object_to_emacs_value(env, SCHEME_CAR(value));
    emacs_value Qcdr = scheme_object_to_emacs_value(env, SCHEME_CDR(value));
    emacs_value args[] = { Qcar, Qcdr };
    return env->funcall(env, Qcons, 2, args);
  }
  if (SCHEME_SYMBOLP(value)) {
    emacs_value ret;
    safe_intern(env, SCHEME_SYM_VAL(value), SCHEME_SYM_LEN(value), &ret);
    return ret;
  }
  return env->intern(env, "null");
}

static racket_emacs_value_type get_emacs_type(emacs_env *env, emacs_value value) {
  emacs_value Qtyp_str = emacs_symbol_name(env, env->type_of(env, value));
  char *buf = emacs_string_to_c_string(env, Qtyp_str);
  racket_emacs_value_type ret = rev_user_ptr;

  // Parse the type string
  switch (buf[0]) {
  case 'i': // Possibly: integer
    if (strncmp(buf, "integer", 7) == 0) {
      ret = rev_integer;
    }
    break;
  case 's': // Possibly: symbol, string
    switch (buf[1]) {
    case 'y': // Possibly: symbol
      if (strncmp(buf, "symbol", 6) == 0) {
	ret = rev_symbol;
      }
      break;
    case 't': // Possibly: string
      if (strncmp(buf, "string", 6) == 0) {
	ret = rev_string;
      }
      break;
    default:
      break;
    }
    break;
  case 'c': // Possibly: cons
    if (strncmp(buf, "cons", 4) == 0) {
      ret = rev_cons;
    }
    break;
  case 'v': // Possibly: vector
    if (strncmp(buf, "vector", 6) == 0) {
      ret = rev_vector;
    }
    break;
  case 'f': // Possibly: float
    if (strncmp(buf, "float", 5) == 0) {
      ret = rev_float;
    }
    break;
  default:
    break;
  }
  
  free(buf);
  return ret;
}

static Scheme_Object *emacs_value_to_scheme_object(emacs_env*, emacs_value);

static Scheme_Object *emacs_symbol_to_scheme_symbol(emacs_env *env, emacs_value value) {
  if (env->eq(env, env->intern(env, "null"), value)) {
    return scheme_make_null();
  } else {
    emacs_value Qstr = emacs_symbol_name(env, value);
    char *buf = emacs_string_to_c_string(env, Qstr);
    Scheme_Object *ret = scheme_intern_symbol(buf);
    free(buf);
    return ret;
  }
}

static Scheme_Object *emacs_string_to_scheme_string(emacs_env *env, emacs_value value) {
  ptrdiff_t bufsize;
  char *buf = emacs_string_to_c_string_with_size(env, value, &bufsize);
  Scheme_Object *ret = scheme_make_sized_utf8_string(buf, bufsize);
  free(buf);
  return ret;
}

static Scheme_Object *emacs_vector_to_scheme_vector(emacs_env *env, emacs_value value) {
  ptrdiff_t vec_size = env->vec_size(env, value);
  ptrdiff_t i;
  Scheme_Object *ret = scheme_make_vector(vec_size, scheme_make_null());
  for (i = 0; i < vec_size; ++i) {
    SCHEME_VEC_ELS(ret)[i] = emacs_value_to_scheme_object(env, env->vec_get(env, value, i));
  }
  return ret;
}

static Scheme_Object *emacs_procedure_to_scheme_procedure(emacs_env *env, emacs_value value) {
  return scheme_make_null();
}

static Scheme_Object *emacs_value_to_scheme_object(emacs_env *env, emacs_value value) {
  racket_emacs_value_type typ = get_emacs_type(env, value);
  emacs_value Qcar, Qcdr, Qcar_val, Qcdr_val;
  switch (typ) {
  case rev_integer:
    return scheme_make_integer_value(env->extract_integer(env, value));
  case rev_float:
    return scheme_make_double(env->extract_float(env, value));
  case rev_symbol:
    return emacs_symbol_to_scheme_symbol(env, value);
  case rev_string:
    return emacs_string_to_scheme_string(env, value);
  case rev_vector:
    return emacs_vector_to_scheme_vector(env, value);
  case rev_procedure:
    return emacs_procedure_to_scheme_procedure(env, value);
  case rev_cons:
    Qcar = env->intern(env, "car");
    Qcdr = env->intern(env, "cdr");
    Qcar_val = env->funcall(env, Qcar, 1, &value);
    Qcdr_val = env->funcall(env, Qcdr, 1, &value);
    return scheme_make_pair(emacs_value_to_scheme_object(env, Qcar_val),
			    emacs_value_to_scheme_object(env, Qcdr_val));
  case rev_user_ptr:
    return scheme_make_null();
  default:
    return scheme_make_null();
  }
}

// performs a lookup of an emacs value or function
static Scheme_Object *racket_emacs_do_symbol_value_or_function(char *name, char *elisp_func, int argc, Scheme_Object *argv[]) {
  if (argc != 1) {
    scheme_wrong_count(name, 1, 1, argc, argv);
  }
  Scheme_Object *sym = argv[0];
  if (!SCHEME_SYMBOLP(sym)) {
    scheme_wrong_type(name, "symbol", 1, argc, argv);
  }
  /* Convert the strings to symbols by interning them */
  emacs_value Qelisp_func = eenv->intern (eenv, elisp_func);
  emacs_value Qsym = eenv->intern (eenv, name);
  emacs_value args[] = { Qsym };
  eenv->funcall(eenv, Qelisp_func, 1, args);
  return scheme_make_null();
}

/* New emacs lisp function. All function exposed to Emacs must have this prototype. */
static emacs_value
Fmymod_test (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  char *argv[] = { "racket" };
  emacs_value ret = env->intern(env, "undefined");
  eenv = env;
  em_val = &ret;
  run_racket(1, argv);
  eenv = NULL;
  em_val = NULL;
  return ret;
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  /* Set the function cell of the symbol named NAME to SFUN using
     the 'fset' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);

  /* Prepare the arguments array */
  emacs_value args[] = { Qsym, Sfun };

  /* Make the call (2 == nb of arguments) */
  env->funcall (env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  /* call 'provide' with FEATURE converted to a symbol */

  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

  /* create a lambda (returns an emacs_value) */
  emacs_value fun = env->make_function (env,
              0,            /* min. number of arguments */
              0,            /* max. number of arguments */
              Fmymod_test,  /* actual function pointer */
              "doc",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );

  bind_function (env, "mymod-test", fun);
  provide (env, "mymod");

  /* loaded successfully */
  return 0;
}
