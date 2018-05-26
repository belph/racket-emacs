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
#include "racket-rtutils.h"
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
  Scheme_Object *ret = scheme_make_sized_utf8_string(buf, bufsize - 1); // skip null
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

Scheme_Object *conv_emacs_pair_to_scheme_pair(emacs_env *env, emacs_value value) {
  Scheme_Object *car = NULL;
  Scheme_Object *cdr = NULL;
  Scheme_Object *ret = NULL;
  MZ_GC_DECL_REG(3);
  MZ_GC_VAR_IN_REG(0, car);
  MZ_GC_VAR_IN_REG(1, cdr);
  MZ_GC_VAR_IN_REG(2, ret);
  MZ_GC_REG();
  car = wrap_emacs_value(env, emacs_car(env, value), false);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  cdr = wrap_emacs_value(env, emacs_cdr(env, value), false);
  EMACS_CHECK_EXIT_UNREG(env, NULL);
  ret = scheme_make_pair(car, cdr);
  MZ_GC_UNREG();
  return ret;
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
  if (SCHEME_FALSEP(value)) {
    return env->intern(env, "nil");
  } else {
    return env->intern(env, "t");
  }
}

emacs_value conv_scheme_pair_to_emacs_pair(emacs_env *env, Scheme_Object *value) {
  emacs_value ret;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, value);
  MZ_GC_REG();
  ret = emacs_cons(env,
                   wrap_racket_value(env, SCHEME_CAR(value)),
                   wrap_racket_value(env, SCHEME_CDR(value)));
  MZ_GC_UNREG();
  return ret;
}

// "Smarter" conversion function
emacs_value conv_scheme_primitive_to_emacs_primitive(emacs_env *env, Scheme_Object *value) {
  if (SCHEME_SYMBOLP(value)) {
    return conv_scheme_symbol_to_emacs_symbol(env, value);
  } else if (SCHEME_INTP(value)) {
    return conv_scheme_integer_to_emacs_integer(env, value);
  } else if (SCHEME_REALP(value)) {
    return conv_scheme_float_to_emacs_float(env, value);
  } else if (SCHEME_CHAR_STRINGP(value) || SCHEME_BYTE_STRINGP(value)) {
    return conv_scheme_string_to_emacs_string(env, value);
  } else if (SCHEME_BOOLP(value)) {
    return conv_scheme_bool_to_emacs_bool(env, value);
  } else if (SCHEME_PAIRP(value)) {
    Scheme_Object *car = SCHEME_CAR(value);
    Scheme_Object *cdr = SCHEME_CDR(value);
    emacs_value ecar, ecdr, ret;
    MZ_GC_DECL_REG(2);
    MZ_GC_VAR_IN_REG(0, car);
    MZ_GC_VAR_IN_REG(1, cdr);
    MZ_GC_REG();
    ecar = conv_scheme_primitive_to_emacs_primitive(env, car);
    EMACS_CHECK_EXIT_UNREG(env, NULL);
    ecdr = conv_scheme_primitive_to_emacs_primitive(env, cdr);
    EMACS_CHECK_EXIT_UNREG(env, NULL);
    ret = emacs_cons(env, ecar, ecdr);
    MZ_GC_UNREG();
    return ret;
  } else if (SCHEME_NULLP(value)) {
    return env->intern(env, "nil");
  } else {
    SCHEME_PRINT_STR("non-primitive: ");
    SCHEME_DISPLAY(value);
    SCHEME_PRINT_STR("\n");
  }
  return wrap_racket_value(env, value);
}
