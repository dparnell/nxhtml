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

(eval-when-compile (require 'web-vcs))


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
    (cond
     ((eq type 'macro)
      (setq type 'defmacro))
     (t
      (setq type 'defun)))
    (eval
     `(web-autoload-1 ,fun ,src ,docstring ,int ,type))))

(defvar web-autoload-default-filename-element nil)

(defcustom web-autoload-autocompile t
  "Byt compile downloaded files if t."
  :type 'boolean
  :group 'web-vcs)

;; Fix-me: change name
(defvar web-auto-load-skip-require-advice nil)

;; Fix-me: Use TYPE
(defmacro web-autoload-1 (fun src docstring interactive type)
  `(progn
     (,type ,fun (&rest args)
       ,(concat docstring
                "\n\nArguments are not yet known since the real function is not loaded."
                "\nFunction is defined by `web-autoload' to be loaded using definition\n\n  "
                (format "%S"
                        src))
       ,interactive
       (let* ((lib-web (or (find-lisp-object-file-name ',fun 'defun)
                           web-autoload-default-filename-element))
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
             (message "web-autoload-1: fun=%s dl-file=%S" ',fun dl-file)
             ;; Fix-me: How to avoid this during byte compiling?
             (unless (file-exists-p dl-file)
               ;; Fix-me: write this function Or, rather use
               ;; web-vcs-get-files-on-page. Make this take another
               ;; arg for matching file name.
               ;;(setq file-url (web-vcs-full-file-url vcs base-url rel-url-el))
               ;;(url-copy-file file-url dl-file nil t) ;; don't overwrite, keep time
               (web-vcs-get-missing-matching-files vcs base-url base-dir rel-url-el)
               (unless (file-exists-p dl-file)
                 (web-vcs-message-with-face 'web-vcs-red "Could not download file %s" dl-file)
                 (throw 'command-level nil))
               (when web-autoload-autocompile
                   (web-autoload-byte-compile-file dl-file t))
               )
             ;; Is it already loaded, or?
             (unless (symbol-function ',fun)
               (let ((dl-file-noel (file-name-sans-extension dl-file)))
                 (load dl-file-noel)
                 (when web-autoload-autocompile
                   (web-autoload-byte-compile-file dl-file t))))
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
                 (if (functionp ',fun)
                     (apply ',fun args)
                   ;; It is a macro
                   (let ((the-macro (append '(,fun) args nil)))
                     (eval the-macro))
                   )))
           (fset ',fun auto-fun)
           (error "web-autoload: %s" err)
           )))
     ))

(defvar web-autoload-require-list nil)

(defun web-autoload-require (feature web-vcs base-url relative-url base-dir)
  "Prepare to download file if necessary when `require' is called.
WEB-VCS BASE-URL RELATIVE-URL"
  (add-to-list 'web-autoload-require-list `(,feature ,web-vcs ,base-url ,relative-url ,base-dir))
  )

;; Fix-me: Set up a byte compilation queue. Move function for byte compiling here.
(defvar web-autoload-compile-queue nil)
(defun web-autoload-byte-compile-file (file load)
  (web-vcs-message-with-face 'web-vcs-gold "Add to compile queue (%S %s)" file load)
  (setq web-autoload-compile-queue (cons (cons file load)
                                         web-autoload-compile-queue))
  (if (< 1 (length web-autoload-compile-queue))
      (throw 'web-autoload-comp-to-top nil)
    (while web-autoload-compile-queue
      (catch 'web-autoload-comp-to-top
        (when (web-autoload-byte-compile-file-1)
          (setq web-autoload-compile-queue (cdr web-autoload-compile-queue)))))))

(defun web-autoload-byte-compile-file-1 ()
  "Compile and load FILE. Or just load."
  (let* ((compiled-it nil)
         (first-entry (car web-autoload-compile-queue))
         (file (car first-entry))
         (load (cdr first-entry))
         (elc-file (byte-compile-dest-file file))
         (need-compile (or (not (file-exists-p elc-file))
                           (file-newer-than-file-p file elc-file))))
      (if (not need-compile)
          (when load
            (load elc-file))
        (condition-case err
            (progn
              (web-vcs-message-with-face 'font-lock-comment-face "Start byte compiling %S" file)
              ;;(when (ad-is-advised 'require) (ad-disable-advice 'require 'around 'web-autoload-ad-require))
              (let ((web-auto-load-skip-require-advice nil))
                (byte-compile-file file load))
              ;;(when (ad-is-advised 'require) (ad-enable-advice 'require 'around 'web-autoload-ad-require))
              (web-vcs-message-with-face 'font-lock-comment-face "Ready byte compiling %S" file))
          (error
           (web-vcs-message-with-face
            'web-vcs-red "Error in byte compiling %S: %s" file (error-message-string err))))))
  ;; Always return t on normal exits to tell to remove the (possibly)
  ;; compiled entry.
  t)

;; Fix-me: protect against deep nesting
(defun web-autoload-do-require (feature filename noerror)
  (let* ((feat-name (symbol-name feature))
         (lib (or filename feat-name)))
    (if (load lib noerror t)
        (progn
          (unless (featurep feature)
            (error "web-autoload: Required feature `%s' was not provided" feature))
          feature)
      nil
      )))

(defadvice require (around
                    web-autoload-ad-require
                                        ;activate
                    ;;compile
                    )
  (let ((feature  (ad-get-arg 0))
        (filename (ad-get-arg 1))
        (noerror  (ad-get-arg 2)))
    (if (featurep feature)
        feature
      (if (and noerror
               (or (not (boundp 'web-auto-load-skip-require-advice))
                   web-auto-load-skip-require-advice))
          (progn
            (message "Doing nearly original require %s, because skipping" (ad-get-arg 0))
            ;; Can't ad-do-it because defadviced functions in load
            (web-autoload-do-require feature filename noerror))
        (let* ((auto-rec (assq feature web-autoload-require-list))
               (web-vcs      (nth 1 auto-rec))
               (base-url     (nth 2 auto-rec))
               (relative-url (nth 3 auto-rec))
               (base-dir     (nth 4 auto-rec)))
          (if (not auto-rec)
              (progn
                (message "Doing nearly original require %s, because no auto-rec" feature)
                (web-autoload-do-require feature filename noerror))
            (message "Doing the really adviced require for %s" feature)
            ;; Check if already downloaded first
            (condition-case err
                (web-autoload-do-require feature filename noerror)
              (error (message "ad-do-it require failed for %s: %s" feature (error-message-string err))))
            (if (featurep feature)
                feature
              ;; Download and try again
              (setq relative-url (concat relative-url ".el"))
              (web-vcs-message-with-face 'font-lock-comment-face "Need to download feature %s (%S %S => %S)" feature base-url relative-url base-dir)
              (catch 'command-level
                (web-vcs-get-missing-matching-files web-vcs base-url base-dir relative-url))
              (web-vcs-message-with-face 'font-lock-comment-face "After downloaded feature %s (%S %S => %S)" feature base-url relative-url base-dir)
              ;; Byte compile the downloaded file
              (let ((dl-file (expand-file-name relative-url base-dir)))
                (when web-autoload-autocompile
                  (web-autoload-byte-compile-file dl-file nil)))
              (web-autoload-do-require feature filename noerror)
              )))))))

(provide 'web-autoload)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-autoload.el ends here
