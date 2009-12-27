;;; web-autoload.el --- Autoload from web site
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-12-26 Sat
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
;; Experimental code. Not ready to use at all.
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

(require 'web-vcs)


(defun web-autoload (fun src docstring interactive type)
  "Set up FUN to be autoloaded from SRC.
This works similar to `autoload' and the arguments DOCSTRING,
INTERACTIVE and TYPE are handled similary.

However loading can be done from a web url.
In that case SRC should have the format

  (WEB-VCS BASE-URL RELATIVE-URL BASE-DIR)

where

  - WEB-VCS is specifies a web repository type, see
    `web-vcs-get-files-from-root'.
  - BASE-URL is the base url, similar to the URL argument to the
    function above.

  - RELATIVE-URL is relative location.  This will be relative to
    BASE-DIR in file tree and to BASE-URL on the web \(only
    logically in the latter case).

Loading will be done from the file resulting from expanding
RELATIVE-URL relative to BASE-DIR.  If this file exists load it
directly, otherwise download it first."
  (let ((int (when interactive '(interactive))))
    (eval
     `(web-autoload-1 ,fun ,src ,docstring ,int ,type))))

;; Fix-me: Use TYPE
(defmacro web-autoload-1 (fun src docstring interactive type)
  `(progn
     (defun ,fun (&rest args)
       ,(concat docstring
                "\n\nArguments are not yet known since the real function is not loaded."
                "\nFunction is defined by `web-autoload' to be loaded using definition\n\n  "
                (format "%S"
                        src))
       ,interactive
       (let* ((lib-web (find-lisp-object-file-name ',fun (symbol-function ',fun)))
              (old-hist-elt (load-history-filename-element lib-web))
              (auto-fun (symbol-function ',fun))
              err)
         (fset ',fun nil)
         (if (not (listp ',src))
             ;; Just a local file, for testing of logics.
             (let ((lib-file (locate-library ',src)))
               (load ',src)
               (unless (symbol-function ',fun)
                 (setq err (format "%s is not in library %s" ',fun lib-file))))
           ;; If file is a list then it should be a web url:
           ;;   (web-vcs base-url relative-url base-dir)
           ;; Convert from repository url to file download url.
           (let* ((vcs      (nth 0 ',src))
                  (base-url (nth 1 ',src))
                  (rel-url  (nth 2 ',src))
                  (base-dir (nth 3 ',src))
                  (rel-url-el (concat rel-url ".el"))
                  file-url
                  dl-file)
             (unless (stringp base-url)
               (setq base-url (symbol-value base-url)))
             (unless (stringp base-dir)
               (setq base-dir (symbol-value base-dir)))
             (setq dl-file (expand-file-name rel-url-el base-dir))
             (message "dl-file=%s" dl-file)
             (unless (file-exists-p dl-file)
               ;; Fix-me: write this function Or, rather use
               ;; web-vcs-get-files-on-page. Make this take another
               ;; arg for matching file name.
               ;;(setq file-url (web-vcs-full-file-url vcs base-url rel-url-el))
               ;;(url-copy-file file-url dl-file nil t) ;; don't overwrite, keep time
               (web-vcs-get-missing-matching-files vcs base-url base-dir rel-url-el)
               (unless (file-exists-p dl-file)
                 (error "Could not download file %s" dl-file))
               )
             (load dl-file)
             (unless (symbol-function ',fun)
               (setq err (format "%s is not in downloaded library %s" ',fun dl-file)))
             ))
         (if (not err)
             (progn
               ;; Delete old load-history entry for ,fun. A new entry
               ;; has been added.
               (let* ((tail (cdr old-hist-elt))
                      (new-tail (when tail (delete (cons 'defun ',fun) tail))))
                 (when tail (setcdr old-hist-elt new-tail)))
               ;; Finally call the real function
               (if (called-interactively-p ',fun)
                   (call-interactively ',fun)
                 (apply ',fun args)))
           (fset ',fun auto-fun)
           (error "web-autoload: %s" err)
           )))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test it
(let ((src (if nil ;; local or remote
               "web-autoload-2"
             `(lp
               ;;,(concat web-vcs-nxhtml-base-url "files/")
               ,(nxhtml-download-root-url nil)
               "util/web-autoload-2" nxhtml-install-dir))))
  (web-autoload 'web-auto2-test9
                    src
                    "test" t nil))

;; (fset 'web-auto2-test9 nil)
;; (web-auto2-test9 "Hi there")
;; (web-auto2-test1 "Hi there")
;; (find-lisp-object-file-name 'web-auto2-test9 nil)
;; (load-history-filename-element "web-autoload")


(provide 'web-autoload)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-autoload.el ends here
