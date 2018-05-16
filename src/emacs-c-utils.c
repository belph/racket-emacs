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

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <emacs-module.h>

#include "emacs-c-utils.h"

char *emacs_string_to_c_string_with_size(emacs_env *env, emacs_value value, ptrdiff_t *size) {
  emacs_value Qthrow_tag, Qthrow_value;
  ptrdiff_t bufsize = 0;
  // Fetch required buffer size
  bool res = env->copy_string_contents(env, value, NULL, &bufsize);
  if (!res || bufsize == 0) {
    Qthrow_tag = env->intern(env, "racket-emacs-internal");
    char zero_msg[] = "Internal error: conv_emacs_string_to_c_string_with_size: Buffer size was zero!";
    Qthrow_value = env->make_string(env, zero_msg, (sizeof(zero_msg) / sizeof(char)) - 1);
    EMACS_CHECK_EXIT(env, NULL);
    env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
    return NULL;
  }
  // Allocate the buffer
  char *buf = malloc(bufsize);
  if (size != NULL) {
    *size = bufsize;
  }
  if (buf == NULL) {
    Qthrow_tag = env->intern(env, "racket-emacs-internal");
    char malloc_msg[] = "Internal error: conv_emacs_string_to_c_string_with_size: Failed to allocate buffer!";
    Qthrow_value = env->make_string(env, malloc_msg, (sizeof(malloc_msg) / sizeof(char)) - 1);
    EMACS_CHECK_EXIT(env, NULL);
    env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
    return NULL;
  }
  // Copy the string into allocated buffer
  if (!env->copy_string_contents(env, value, buf, &bufsize)) {
    Qthrow_tag = env->intern(env, "racket-emacs-internal");
    char copy_msg[] = "Internal error: conv_emacs_string_to_c_string_with_size: Failed to copy string contents!";
    Qthrow_value = env->make_string(env, copy_msg, (sizeof(copy_msg) / sizeof(char)) - 1);
    EMACS_CHECK_EXIT(env, NULL);
    env->non_local_exit_throw(env, Qthrow_tag, Qthrow_value);
    return NULL;
  }
  return buf;
}

char *emacs_string_to_c_string(emacs_env *env, emacs_value value) {
  return emacs_string_to_c_string_with_size(env, value, NULL);
}

emacs_value emacs_symbol_name(emacs_env *env, emacs_value Qsym) {
  emacs_value Qsymbol_name = env->intern(env, "symbol-name");
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value args[] = { Qsym };
  return env->funcall(env, Qsymbol_name, 1, args);
}

// Source: http://phst.github.io/emacs-modules.html#interning
bool emacs_safe_intern(emacs_env *env, const char *name, size_t size, emacs_value *result) {
  bool simple = true;
  for (size_t i = 0; i < size; ++i) {
    if (name[i] == '\0' || !isascii (name[i])) {
      simple = false;
      break;
    }
  }
  if (simple) {
    *result = env->intern (env, name);
  } else {
    emacs_value string_object = env->make_string(env, name, (ptrdiff_t) size);
    *result = env->funcall(env, env->intern (env, "intern"),
			   1, &string_object);
  }
  return env->non_local_exit_check (env) == emacs_funcall_exit_return;
}
