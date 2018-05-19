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

#include <stdlib.h>

#include <re-config.h>
#include <scheme.h>
#include <emacs-module.h>

#include "emacs-c-utils.h"
#include "conv.h"

// Emacs -> Racket

Scheme_Object *conv_emacs_integer_to_scheme_integer(emacs_env *env, emacs_value value) {
  return scheme_make_integer_value(env->extract_integer(env, value));
}

Scheme_Object *conv_emacs_float_to_scheme_float(emacs_env *env, emacs_value value) {
  return scheme_make_double(env->extract_float(env, value));
}

Scheme_Object *conv_emacs_symbol_to_scheme_symbol(emacs_env *env, emacs_value value) {
  emacs_value Qstr = emacs_symbol_name(env, value);
  EMACS_CHECK_EXIT(env, NULL);
  char *buf = emacs_string_to_c_string(env, Qstr);
  EMACS_CHECK_EXIT(env, NULL);
  Scheme_Object *ret = scheme_intern_symbol(buf);
  free(buf);
  return ret;
}

Scheme_Object *conv_emacs_string_to_scheme_string(emacs_env *env, emacs_value value) {
  ptrdiff_t bufsize;
  char *buf = emacs_string_to_c_string_with_size(env, value, &bufsize);
  EMACS_CHECK_EXIT(env, NULL);
  Scheme_Object *ret = scheme_make_sized_utf8_string(buf, bufsize);
  free(buf);
  return ret;
}

Scheme_Object *conv_emacs_bool_to_scheme_bool(emacs_env *env, emacs_value value) {
  if (env->eq(env, value, env->intern(env, "nil"))) {
    return scheme_false;
  } else {
    return scheme_true;
  }
}

// Racket -> Emacs

emacs_value conv_scheme_integer_to_emacs_integer(emacs_env *env, Scheme_Object *value) {
  return env->make_integer(env, (intmax_t)SCHEME_INT_VAL(value));
}

emacs_value conv_scheme_float_to_emacs_float(emacs_env *env, Scheme_Object *value) {
  return env->make_float(env, SCHEME_DBL_VAL(value));
}

emacs_value conv_scheme_symbol_to_emacs_symbol(emacs_env *env, Scheme_Object *value) {
  emacs_value ret;
  emacs_safe_intern(env, SCHEME_SYM_VAL(value), SCHEME_SYM_LEN(value), &ret);
  return ret;
}

emacs_value conv_scheme_string_to_emacs_string(emacs_env *env, Scheme_Object *value) {
  if (SCHEME_CHAR_STRINGP(value)) {
    value = scheme_char_string_to_byte_string(value);
  }
  char *buf = SCHEME_BYTE_STR_VAL(value);
  ptrdiff_t bytelen = SCHEME_BYTE_STRLEN_VAL(value);
  return env->make_string(env, buf, bytelen);
}

emacs_value conv_scheme_bool_to_emacs_bool(emacs_env *env, Scheme_Object *value) {
  if (scheme_equal(scheme_false, value)) {
    return env->intern(env, "nil");
  } else {
    return env->intern(env, "t");
  }
}
