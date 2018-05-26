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

emacs_value emacs_symbol_value(emacs_env *env, emacs_value Qsym) {
  emacs_value Qsymbol_value = env->intern(env, "symbol-value");
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value args[] = { Qsym };
  return env->funcall(env, Qsymbol_value, 1, args);
}

emacs_value emacs_symbol_function(emacs_env *env, emacs_value Qsym) {
  emacs_value Qsymbol_function = env->intern(env, "symbol-function");
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value args[] = { Qsym };
  return env->funcall(env, Qsymbol_function, 1, args);
}

emacs_value emacs_car(emacs_env *env, emacs_value Qsym) {
  emacs_value Qcar = env->intern(env, "car");
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value args[] = { Qsym };
  return env->funcall(env, Qcar, 1, args);
}

emacs_value emacs_cdr(emacs_env *env, emacs_value Qsym) {
  emacs_value Qcdr = env->intern(env, "cdr");
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value args[] = { Qsym };
  return env->funcall(env, Qcdr, 1, args);
}

emacs_value emacs_cons(emacs_env *env, emacs_value Qcar, emacs_value Qcdr) {
  emacs_value Qcons = env->intern(env, "cons");
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value args[] = { Qcar, Qcdr };
  return env->funcall(env, Qcons, 2, args);
}

bool emacs_boundp(emacs_env *env, emacs_value Qsym) {
  emacs_value Qsymbol_boundp = env->intern(env, "boundp");
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value args[] = { Qsym };
  emacs_value ret = env->funcall(env, Qsymbol_boundp, 1, args);
  EMACS_CHECK_EXIT(env, NULL);
  return env->is_not_nil(env, ret);
}

bool emacs_fboundp(emacs_env *env, emacs_value Qsym) {
  emacs_value Qsymbol_fboundp = env->intern(env, "fboundp");
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value args[] = { Qsym };
  emacs_value ret = env->funcall(env, Qsymbol_fboundp, 1, args);
  EMACS_CHECK_EXIT(env, NULL);
  return env->is_not_nil(env, ret);
}

emacs_value emacs_format(emacs_env *env, const char *format_str, ptrdiff_t argc, emacs_value rest_args[]) {
  emacs_value Qsymbol_format = env->intern(env, "format");
  emacs_value args[argc + 1];
  int idx;
  EMACS_CHECK_EXIT(env, NULL);
  args[0] = env->make_string(env, format_str, strlen(format_str));
  EMACS_CHECK_EXIT(env, NULL);
  for (idx = 0; idx < argc; ++idx) {
    args[idx + 1] = rest_args[idx];
  }
  return env->funcall(env, Qsymbol_format, argc + 1, args);
}

emacs_value emacs_message(emacs_env *env, const char *format_str, ptrdiff_t argc, emacs_value rest_args[]) {
  emacs_value Qsymbol_message = env->intern(env, "message");
  emacs_value args[argc + 1];
  int idx;
  EMACS_CHECK_EXIT(env, NULL);
  args[0] = env->make_string(env, format_str, strlen(format_str));
  EMACS_CHECK_EXIT(env, NULL);
  for (idx = 0; idx < argc; ++idx) {
    args[idx + 1] = rest_args[idx];
  }
  //fprintf(stderr, "Calling message with format: '%s'; argc: '%s'; ")
  return env->funcall(env, Qsymbol_message, argc + 1, args);
}

emacs_value emacs_load_file_directory(emacs_env *env) {
  emacs_value Qsymbol_load_file_name = env->intern(env, "load-file-name");
  emacs_value Qfile_name_directory = env->intern(env, "file-name-directory");
  emacs_value Qfile_truename = env->intern(env, "file-truename");
  EMACS_CHECK_EXIT(env, NULL);
  if (!emacs_boundp(env, Qsymbol_load_file_name)) {
    return env->intern(env, "nil");
  }
  emacs_value Qload_file_name = emacs_symbol_value(env, Qsymbol_load_file_name);
  EMACS_CHECK_EXIT(env, NULL);
  emacs_value Qload_file_directory = env->funcall(env, Qfile_name_directory, 1, &Qload_file_name);
  EMACS_CHECK_EXIT(env, NULL);
  return env->funcall(env, Qfile_truename, 1, &Qload_file_directory);
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

bool emacs_is_user_ptr(emacs_env *env, emacs_value value) {
  emacs_value Quser_ptr = env->intern(env, "user-ptr");
  EMACS_CHECK_EXIT(env, false);
  emacs_value Qtyp = env->type_of(env, value);
  EMACS_CHECK_EXIT(env, false);
  return env->eq(env, Quser_ptr, Qtyp);
}

/* Bind NAME to VAL.  */
void bind_value(emacs_env *env, const char *name, emacs_value Qval) {
  /* Set the function cell of the symbol named NAME to QVAL using
     the 'set' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qset = env->intern (env, "set");
  emacs_value Qsym = env->intern (env, name);

  /* Prepare the arguments array */
  emacs_value args[] = { Qsym, Qval };

  /* Make the call (2 == nb of arguments) */
  env->funcall (env, Qset, 2, args);
}

/* Bind NAME to FUN.  */
void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
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
void provide(emacs_env *env, const char *feature) {
  /* call 'provide' with FEATURE converted to a symbol */

  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}
