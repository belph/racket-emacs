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
 * @file conv.h
 * @author Philip Blair
 * @date 14 May 2018
 * @brief Conversion functions between Racket and Emacs primitives
 */


#ifndef RKT_EMACS_CONV_H
#define RKT_EMACS_CONV_H

#include <re-config.h>
#include <scheme.h>
#include <emacs-module.h>

// Emacs values -> Racket values

Scheme_Object *conv_emacs_integer_to_scheme_integer(emacs_env *env, emacs_value value);
Scheme_Object *conv_emacs_float_to_scheme_float(emacs_env *env, emacs_value value);
Scheme_Object *conv_emacs_symbol_to_scheme_symbol(emacs_env *env, emacs_value value);
Scheme_Object *conv_emacs_string_to_scheme_string(emacs_env *env, emacs_value value);
Scheme_Object *conv_emacs_bool_to_scheme_bool(emacs_env *env, emacs_value value);


// Racket values -> Emacs values

emacs_value conv_scheme_integer_to_emacs_integer(emacs_env *env, Scheme_Object *value);
emacs_value conv_scheme_float_to_emacs_float(emacs_env *env, Scheme_Object *value);
emacs_value conv_scheme_symbol_to_emacs_symbol(emacs_env *env, Scheme_Object *value);
emacs_value conv_scheme_string_to_emacs_string(emacs_env *env, Scheme_Object *value);
emacs_value conv_scheme_bool_to_emacs_bool(emacs_env *env, Scheme_Object *value);

#endif // RKT_EMACS_CONV_H
