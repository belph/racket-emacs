;;; basic-test --- Test basic module loading

;; Copyright (C) 2018 Philip Blair

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
;;; Code:

(require 'ert)
(require 'racket-emacs)
(require 'test-config)

(ert-deftest basic-test-require ()
  (should (require 'racket-emacs)))

(ert-deftest basic-test-eval-racket-file ()
  (let ((filename (racket-emacs-test-data-file "tst.rkt")))
    (message "Loading Racket file: %s" filename)
    (eval-racket-file filename)))

(ert-deftest basic-test-load-symbol ()
  (let ((imported (eval-racket-file (racket-emacs-test-data-file "tst.rkt") 'val)))
    (should (eq 'foobar (racket-emacs/unwrap-symbol imported)))))

(ert-deftest basic-test-raw-car ()
  (should (eq (racket-emacs/runtime/call-raw racket-emacs/raw/car racket-tst-list) 'firstval)))

(ert-deftest basic-test-call-add ()
  (let* ((+ (racket-emacs/runtime/call-raw racket-emacs/raw/dynamic-require
                                           (racket-emacs/wrap-symbol 'racket/base)
                                           (racket-emacs/wrap-symbol '+)))
         (res (racket-emacs/runtime/call-raw +
                                             (racket-emacs/wrap-integer 2)
                                             (racket-emacs/wrap-integer 3)))
         (res (racket-emacs/unwrap-integer res)))
    (should (eql 5 res))))

(ert-deftest basic-test-pprint ()
  (racket-emacs/pretty-print (racket-emacs/wrap-integer 2)))

(ert-deftest basic-test-defracket ()
  (defracket racket/base (racket+ . +))
  (should (eql 5 (racket+ 2 3))))

;;; basic-test.el ends here



