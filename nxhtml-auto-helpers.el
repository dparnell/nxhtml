;;; nxhtml-auto-helpers.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-12-27 Sun
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'web-autoload)
(require 'web-vcs)

(defvar nxhtml-autoload-web nil
  "If t download from web if necessary.")

(defun nxhtml-autoload (fun src &optional docstring interactive type)
  (if nxhtml-autoload-web
      (web-autoload fun src docstring interactive type)
    (let ((file src))
      (when (listp file)
        (setq file (file-name-nondirectory (nth 2 file))))
      (autoload fun file docstring interactive type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test it
(when nil
  (let ((src (if nil ;; local or remote
                 "web-autoload-2"
               `(lp
                 ;;,(concat web-vcs-nxhtml-base-url "files/")
                 ,(nxhtml-download-root-url nil)
                 "util/web-autoload-2" nxhtml-install-dir))))
    (web-autoload 'web-auto2-test9
                  src
                  "test" t nil))
  )

;; (fset 'web-auto2-test9 nil)
;; (web-auto2-test9 "Hi there")
;; (web-auto2-test1 "Hi there")
;; (find-lisp-object-file-name 'web-auto2-test9 nil)
;; (load-history-filename-element "web-autoload")

(provide 'nxhtml-auto-helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-auto-helpers.el ends here
