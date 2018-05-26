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
 * @file emacs-c-utils.h
 * @author Philip Blair
 * @date 14 May 2018
 * @brief Convenience utilities for Emacs C bindings
 */

#ifndef RKT_EMACS_EMACS_C_UTILS_H
#define RKT_EMACS_EMACS_C_UTILS_H
#include <emacs-module.h>

#define STRLEN(x) ((sizeof(x) / sizeof(char)) - 1)
// https://stackoverflow.com/a/2124433
#define NUMEMACS_ARGS(...)  (sizeof((emacs_value[]){NULL, ##__VA_ARGS__})/sizeof(emacs_value) - 1)

#define EMACS_CHECK_EXIT(env, retval)                                   \
  do {                                                                  \
    emacs_env *___eenv_exit = env;                                           \
    if (___eenv_exit->non_local_exit_check(___eenv_exit) != emacs_funcall_exit_return) { \
      return retval;                                                    \
    }                                                                   \
  } while (0)

#define EMACS_CHECK_EXIT_UNREG(env, retval)                             \
  do {                                                                  \
    emacs_env *___eenv_exit = env;                                           \
    if (___eenv_exit->non_local_exit_check(___eenv_exit) != emacs_funcall_exit_return) { \
      MZ_GC_UNREG();                                                    \
      return retval;                                                    \
    }                                                                   \
  } while (0)

// Creates an emacs_value string from the given constant
#define EMACS_STRING(env, s) \
  ({                                                         \
    emacs_env *___eenv_str = env;                            \
    char __s[] = "" s; /* force to be string literal */      \
    ptrdiff_t __len = STRLEN(__s);                               \
    ___eenv_str->make_string(___eenv_str, __s, __len);           \
  })

#define EMACS_EXN(env, tag, value) \
  do {                             \
    emacs_env *___eenv_exn = env;                                   \
    emacs_value Qthrow_tag = ___eenv_exn->intern(___eenv_exn, tag);     \
    emacs_value Qthrow_value = EMACS_STRING(___eenv_exn, value);        \
    if (___eenv_exn->non_local_exit_check(___eenv_exn) == emacs_funcall_exit_return) { \
      ___eenv_exn->non_local_exit_throw(___eenv_exn, Qthrow_tag, Qthrow_value); \
    }                                                                   \
  } while (0)

#define EMACS_MESSAGE(env, format, ...)                   \
  do {                                                    \
    emacs_value __msg_args[] = {__VA_ARGS__};             \
    emacs_message(env, format, NUMEMACS_ARGS(__VA_ARGS__), __msg_args); \
  } while (0)

/**
 * @brief Convert an Emacs string to a C string and save its size
 *
 * Note: The result from this function must be freed.
 * 
 * @param env The Emacs environment
 * @param value The Emacs value
 * @param size A pointer to place the size of the C string (in bytes) into. This may be @verbatim null@endverbatim
 * @return The C string
 * @see emacs_string_to_c_string
 */
char *emacs_string_to_c_string_with_size(emacs_env *env, emacs_value value, ptrdiff_t *size);

/**
 * @brief Convenience version of #emacs_string_to_c_string_with_size
 *
 * This is equivalent to the following:
 * @code
 * emacs_string_to_c_string_with_size(env, value, NULL)
 * @endcode
 *
 * Note: The result from this function must be freed.
 * @return The C string
 */
char *emacs_string_to_c_string(emacs_env *env, emacs_value value);

/**
 * @brief Wrapper for @verbatim symbol-name@endverbatim
 * @return The Emacs string containing the given symbol's name
 */
emacs_value emacs_symbol_name(emacs_env *env, emacs_value sym);

/**
 * @brief Wrapper for @verbatim symbol-value@endverbatim
 * @return The Emacs value bound to the symbol's value slot
 */
emacs_value emacs_symbol_value(emacs_env *env, emacs_value sym);

/**
 * @brief Wrapper for @verbatim symbol-function@endverbatim
 * @return The Emacs value bound to the symbol's function slot
 */
emacs_value emacs_symbol_function(emacs_env *env, emacs_value sym);

/**
 * @brief Wrapper for @verbatim car@endverbatim
 * @return The Emacs value in the car position
 */
emacs_value emacs_car(emacs_env *env, emacs_value value);

/**
 * @brief Wrapper for @verbatim cdr@endverbatim
 * @return The Emacs value in the cdr position
 */
emacs_value emacs_cdr(emacs_env *env, emacs_value value);

/**
 * @brief Wrapper for @verbatim cons@endverbatim
 * @return The Emacs pair
 */
emacs_value emacs_cons(emacs_env *env, emacs_value Qcar, emacs_value Qcdr);

/**
 * @brief Wrapper for @verbatim boundp@endverbatim
 * @return Whether the symbol is bound
 */
bool emacs_boundp(emacs_env *env, emacs_value Qsym);

/**
 * @brief Wrapper for @verbatim fboundp@endverbatim
 * @return Whether the symbol is bound
 */
bool emacs_fboundp(emacs_env *env, emacs_value Qsym);

/**
 * @brief Wrapper for @verbatim format@endverbatim
 * @return The formatted Emacs string
 */
emacs_value emacs_format(emacs_env *env, const char *format, ptrdiff_t argc, emacs_value args[]);

/**
 * @brief Wrapper for @verbatim message@endverbatim
 * @return The message
 */
emacs_value emacs_message(emacs_env *env, const char *format, ptrdiff_t argc, emacs_value args[]);

/**
 * @brief Fetch the directory of the current file
 */
emacs_value emacs_load_file_directory(emacs_env *env);

/**
 * @brief UTF-aware wrapper for Emacs interning
 * @return 
 */
bool emacs_safe_intern(emacs_env *env, const char *name, size_t size, emacs_value *result);

/**
 * @brief Check whether the given value is an Emacs user pointer
 * @return Whether it is a user pointer
 */
bool emacs_is_user_ptr(emacs_env *env, emacs_value value);

/**
 * @brief Bind NAME to VAL.
 */
void bind_value(emacs_env *env, const char *name, emacs_value Qval);

/**
 * @brief Bind NAME to FUN.
 */
void bind_function(emacs_env *env, const char *name, emacs_value Sfun);

/**
 * @brief Provide FEATURE to Emacs.
 */
void provide(emacs_env *env, const char *feature);

#endif
