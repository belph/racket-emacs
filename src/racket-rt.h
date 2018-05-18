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

/**
 * @file racket-rt.h
 * @author Philip Blair
 * @date 14 May 2018
 * @brief Racket runtime basic functions
 */

#ifndef RKT_EMACS_RACKET_RTSUPPORT_H
#define RKT_EMACS_RACKET_RTSUPPORT_H

// these #ifdef's are taken from VIM (see comment in racket-rtsupport.c)
#ifdef __MINGW32__
/* Hack to engage Cygwin-specific settings */
#define __CYGWIN32__
#include <stdint.h>
#endif

#ifdef PROTO
/* avoid syntax error for defining Thread_Local_Variables. */
#define __thread /* empty */
#endif

#include <re-config.h>
#include <schvers.h>
#include <scheme.h>

#ifdef __MINGW32__
#undef __CYGWIN32__
#endif

#if MZSCHEME_VERSION_MAJOR >= 299
#define SCHEME_STRINGP(obj) (SCHEME_BYTE_STRINGP(obj) || SCHEME_CHAR_STRINGP(obj))
#define BYTE_STRING_VALUE(obj) ((char_u *)SCHEME_BYTE_STR_VAL(obj))
#else
/* macros for compatibility with older versions */
#define scheme_current_config() scheme_config
#define scheme_make_sized_byte_string scheme_make_sized_string
#define scheme_format_utf8 scheme_format
#ifndef DYNAMIC_MZSCHEME
/* for dynamic MzScheme there will be separate definitions in if_mzsch.c */
#define scheme_get_sized_byte_string_output scheme_get_sized_string_output
#define scheme_make_byte_string scheme_make_string
#define scheme_make_byte_string_output_port scheme_make_string_output_port
#endif

#define SCHEME_BYTE_STRLEN_VAL SCHEME_STRLEN_VAL
#define BYTE_STRING_VALUE(obj) ((char_u *)SCHEME_STR_VAL(obj))
#define scheme_byte_string_to_char_string(obj) (obj)
#define SCHEME_BYTE_STRINGP SCHEME_STRINGP
#endif

/* Precise GC macros */
#ifndef MZ_GC_DECL_REG
#define MZ_GC_DECL_REG(size)		 /* empty */
#endif
#ifndef MZ_GC_VAR_IN_REG
#define MZ_GC_VAR_IN_REG(x, v)		 /* empty */
#endif
#ifndef MZ_GC_ARRAY_VAR_IN_REG
#define MZ_GC_ARRAY_VAR_IN_REG(x, v, l) /* empty */
#endif
#ifndef MZ_GC_REG
#define MZ_GC_REG()			 /* empty */
#endif
#ifndef MZ_GC_UNREG
#define MZ_GC_UNREG()			 /* empty */
#endif

#ifdef MZSCHEME_FORCE_GC
/*
 * force garbage collection to check all references are registered
 * seg faults will indicate not registered refs
 */
#define MZ_GC_CHECK() scheme_collect_garbage();
#else
#define MZ_GC_CHECK()			/* empty */
#endif

#ifndef thread_local
#define thread_local _Thread_local
#endif

// From VIM:
/*
 * scheme_register_tls_space is only available on 32-bit Windows until
 * racket-6.3.  See
 * http://docs.racket-lang.org/inside/im_memoryalloc.html?q=scheme_register_tls_space
 */
#if MZSCHEME_VERSION_MAJOR >= 500 && defined(WIN32) \
        && defined(USE_THREAD_LOCAL) \
        && (!defined(_WIN64) || MZSCHEME_VERSION_MAJOR >= 603)
#define RKT_NEEDS_TLS_SPACE 1
#endif

/*
 * Since version 4.x precise GC requires trampolined startup.
 * Futures and places in version 5.x need it too.
 */
#if defined(MZ_PRECISE_GC) && MZSCHEME_VERSION_MAJOR >= 400 \
    || MZSCHEME_VERSION_MAJOR >= 500 \
        && (defined(MZ_USE_FUTURES) || defined(MZ_USE_PLACES))
#define TRAMPOLINED_RACKET_STARTUP
#endif

#define RACKET_INIT_RET(env, err_ret)                                   \
  do {                                                                  \
    emacs_env *__env = env;                                             \
    if (racket_init(__env)) {                                           \
      EMACS_CHECK_EXIT(__env, err_ret);                                 \
      emacs_value Qthrow_tag = __env->intern(__env, "racket-emacs");    \
      char msg[] = "Failed to initialize Racket runtime";               \
      emacs_value Qthrow_value = __env->make_string(__env, msg, STRLEN(msg)); \
      __env->non_local_exit_throw(__env, Qthrow_tag, Qthrow_value);     \
      return err_ret;                                                   \
    }                                                                   \
  } while (0)
  
#define SCHEME_EMACSVALUEP(obj) SAME_TYPE(SCHEME_TYPE(obj), mz_emacs_type)
#define SCHEME_EMACSENVP(obj) SAME_TYPE(SCHEME_TYPE(obj), mz_emacs_env_type)

#define SCHEME_EMACSVALUE_DEREF(obj) (((emacs_mz_value*)obj)->value)
#define SCHEME_EMACSENV_DEREF(obj) (((emacs_mz_env*)obj)->env)

Scheme_Type mz_emacs_type;
Scheme_Type mz_emacs_env_type;

typedef struct {
  Scheme_Object so;
  emacs_value value;
} emacs_mz_value;

typedef struct {
  Scheme_Object so;
  emacs_env *env;
} emacs_mz_env;

Scheme_Env *get_racket_env();
emacs_value Feval_racket_file(emacs_env *env, ptrdiff_t argc, emacs_value argv[], void *data);
int racket_init(emacs_env *env);
int racket_main();

#endif // RKT_EMACS_RACKET_RTSUPPORT_H
