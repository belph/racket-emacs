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
(defconst racket-emacs--directory (file-name-directory load-file-name))

(unless (require 'libracketemacs nil :noerror)
  (add-to-list 'load-path racket-emacs--directory)
  (require 'libracketemacs))


(provide 'racket-emacs)

;;; racket-emacs.el ends here
