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

#define EMACS_CHECK_EXIT(env, retval)                                   \
  do {                                                                  \
    emacs_env *___eenv = env;                                           \
    if (___eenv->non_local_exit_check(___eenv) != emacs_funcall_exit_return) { \
      return retval;                                                    \
    }                                                                   \
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
 * @brief UTF-aware wrapper for Emacs interning
 * @return 
 */
bool emacs_safe_intern(emacs_env *env, const char *name, size_t size, emacs_value *result);

#endif
