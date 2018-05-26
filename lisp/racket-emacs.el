;;; racket-emacs --- Racket Scripting in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Philip Blair

;; Author: Philip Blair
;; Maintainer: Philip Blair <philip@pblair.org>
;; URL: https://github.com/belph/racket-emacs
;; Version: 0.0.1
;; Keywords: extensions

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package adds support for scripting Emacs with Racket via an
;; embedded Racket runtime.  This is facilitated by Emacs' dynamic module
;; interface; as such, this package requires an Emacs build which has
;; been configured appropriately.
;;

;;; Code:

(require 'cl-lib)

(defconst racket-emacs--directory (file-name-directory load-file-name))
(defconst racket-emacs--collects-dir (file-truename (concat racket-emacs--directory "/collects")))

(unless (require 'libracketemacs nil :noerror)
  (add-to-list 'load-path racket-emacs--directory)
  (require 'libracketemacs))

(defun racket-emacs--wrap-primitive (x)
  "Convert X to its wrapped Racket primitive representation, as possible.
NIL values will be converted to booleans."
  (cond
   ((eq x nil) racket-emacs/null)
   ((integerp x) (racket-emacs/wrap-integer x))
   ((floatp x) (racket-emacs/wrap-float x))
   ((symbolp x) (racket-emacs/wrap-symbol x))
   ((stringp x) (racket-emacs/wrap-string x))
   ((consp x) (racket-emacs/wrap-cons (cons (racket-emacs--wrap-primitive (car x))
                                            (racket-emacs--wrap-primitive (cdr x)))))
   (t x)))

(defun racket-emacs--wrapped-apply (func args)
  "Apply FUNC with ARGS, wrapping primitive values as possible."
  (message "applying %S with args %S" func args)
  (racket-emacs--unwrap-primitive
   (apply 'racket-emacs/runtime/call-raw (cons func (mapcar 'racket-emacs--wrap-primitive args)))))

(defun racket-emacs--wrapped-func (func)
  "Create a lambda-wrapped version of FUNC."
  (message "wrapping %S" func)
  (lambda (&rest args) (message "Applying %S with args: %S" func args) (racket-emacs--wrapped-apply func args)))

;; draft version:
(defun racket-emacs/dynamic-require (mod &rest identifiers)
  "Require IDENTIFIERS from Racket module MOD.  The results are returned as an association list."
  (message "dynreq %S %S" mod identifiers)
  (unless (symbolp mod)
    (signal 'wrong-type-argument (format "Module should be a symbol. Given: %S" mod)))
  (let* ((dynreq (racket-emacs--wrapped-func racket-emacs/raw/dynamic-require)))
    (mapcar #'(lambda (id)
                (unless (or (symbolp id)
                            (and (consp id)
                                 (symbolp (car id))
                                 (symbolp (cdr id))))
                  (signal 'wrong-type-argument (format "Identifier should be a symbol. Given: %S" id)))
                (let ((in-id (if (consp id) (cdr id) id))
                      (out-id (if (consp id) (car id) id)))
                  (cons out-id (funcall dynreq mod in-id))))
            identifiers)))

(defun re-cl-prettyprint (form)
  "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
  (let ((pt (point)) last)
    (insert "\n" (prin1-to-string form) "\n")
    (setq last (point))
    (goto-char (1+ pt))
    (replace-string "\n\"" "\\n\"")
    (while (search-forward "(quote " last t)
      (delete-char -7)
      (insert "'")
      (forward-sexp)
      (delete-char 1))
    (goto-char (1+ pt))
    (cl--do-prettyprint)))

(defun cl--do-prettyprint ()
  (skip-chars-forward " ")
  (if (looking-at "(")
      (let ((skip (or (looking-at "((") (looking-at "(prog")
		                  (looking-at "(unwind-protect ")
		                  (looking-at "(function (")
		                  (looking-at "(cl--block-wrapper ")))
	          (two (or (looking-at "(defun ") (looking-at "(defmacro ")))
	          (let (or (looking-at "(let\\*? ") (looking-at "(while ") (looking-at "(dolist ")))
	          (set (looking-at "(p?set[qf] ")))
	      (if (or skip let
		            (progn
		              (forward-sexp)
		              (and (>= (current-column) 40) (progn (backward-sexp) t))))
	          (let ((nl t))
	            (forward-char 1)
	            (cl--do-prettyprint)
	            (or skip (looking-at ")") (cl--do-prettyprint))
	            (or (not two) (looking-at ")") (cl--do-prettyprint))
	            (while (not (looking-at ")"))
		            (if set (setq nl (not nl)))
		            (if nl (insert "\n"))
		            (lisp-indent-line)
		            (cl--do-prettyprint))
	            (forward-char 1))))
    (forward-sexp)))

(defun pprint (form &optional output-stream)
  "Pretty-print FORM to OUTPUT-STREAM.  Used for debugging."
  (princ (with-temp-buffer
           (re-cl-prettyprint form)
           (buffer-string))
         output-stream)
  (princ "\n" output-stream))

(defun pprint-macroexpand (form &optional output-stream)
  (pprint (macroexpand form) output-stream))

(defun racket-emacs/pretty-print (value)
  "Pretty-print VALUE to STDOUT."
  (let ((bind (racket-emacs/dynamic-require 'racket/pretty 'pretty-print)))
    (racket-emacs--wrapped-apply (cdar bind) (list value))))

(defun racket-emacs/runtime/expand-requires (spec)
  (racket-emacs--unwrap-primitive
   (racket-emacs/runtime/raw/expand-requires
    (racket-emacs--wrap-primitive spec))))

;;(defmacro defracket (mod &rest identifiers)
;;  "Defines IDENTIFIERS by wrapping the expored Racket values from MOD."
;;  (let* ((mapped (mapcar (lambda (id) `',id) identifiers)))
;;    `(dolist (bind (racket-emacs/dynamic-require ',mod . ,mapped))
;;       (let ((out-id (car bind))
;;             (val (cdr bind)))
;;         (set out-id val)
;;         (fset out-id (racket-emacs--wrapped-func val))))))

(defmacro defracket (spec)
  "Import the values listed in the Racket require specification SPEC."
  `(dolist (bind (racket-emacs/runtime/expand-requires ',spec))
     (let ((name (car bind))
           (import-type (cadr bind))
           (value (cddr bind)))
       (cond ((eq import-type 'value)
              (set name value)
              (fset name (racket-emacs--wrapped-func value)))
             ((eq import-type 'macro)
              (error "Macro imports are unsupported"))
             (t (error "Unknown import type: %S" (cadr bind)))))))

(defmacro let-racket (binds &rest body)
  "Create local bindings of the Racket values specified in BINDS in the scope of BODY."
  nil)

(provide 'racket-emacs)

;;; racket-emacs.el ends here
