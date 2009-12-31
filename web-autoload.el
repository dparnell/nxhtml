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

;;(eval-when-compile (require 'web-vcs))


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
         ;; Fix-me: Can't do this because we may have to go back here again...
         ;;(fset ',fun nil)
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
             (web-vcs-message-with-face 'web-vcs-gold "web-autoload-1: BEG fun=%s dl-file=%S" ',fun dl-file)
             ;; Fix-me: How to avoid this during byte compiling?
             (unless (file-exists-p dl-file)
               (web-vcs-get-missing-matching-files vcs base-url base-dir rel-url-el)
               (unless (file-exists-p dl-file)
                 (web-vcs-message-with-face 'web-vcs-red "Could not download file %s" dl-file)
                 (throw 'command-level nil)))
             (when web-autoload-autocompile
               (web-autoload-byte-compile-file dl-file t))
             ;; Is it already loaded, or?
             ;; Fix-me: rethink!
             (unless nil ;(symbol-function ',fun)
               (let ((dl-file-noel (file-name-sans-extension dl-file)))
                 (load dl-file-noel)))
             (unless (symbol-function ',fun)
               (setq err (format "%s is not in downloaded library %s" ',fun dl-file)))
             (web-vcs-message-with-face 'web-vcs-gold "web-autoload-1: END fun=%s dl-file=%S" ',fun dl-file)
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

(defvar web-autoload-cleanup-dummy-el
  (let* ((this-dir (file-name-directory (or load-file-name
                                            (when (boundp 'bytecomp-filename) bytecomp-filename)
                                            buffer-file-name))))
    (expand-file-name "temp-cleanup.el" this-dir)))

(defun web-autoload-try-cleanup-after-failed-compile ()
  (let* ((bc-input-buffer (get-buffer " *Compiler Input*"))
         (bc-outbuffer (get-buffer " *Compiler Output*"))
         (active-comp (car web-autoload-compile-queue))
         (active-file (car active-comp))
         (active-elc (byte-compile-dest-file active-file)))
    ;; Delete bytecomp buffers
    (web-vcs-message-with-face 'web-vcs-gold "Trying to cleanup %s %s %s" bc-input-buffer bc-outbuffer active-elc)
    (when bc-input-buffer (kill-buffer bc-input-buffer))
    (when bc-outbuffer
      (kill-buffer bc-outbuffer)
      (setq bytecomp-outbuffer nil))
    ;; Delete half finished elc file
    (when (file-exists-p active-elc)
      (delete-file active-elc))
    (setq byte-compile-constants nil)
    (setq byte-compile-variables nil)
    (setq byte-compile-bound-variables nil)
    (setq byte-compile-const-variables nil)
    ;;(setq byte-compile-macro-environment byte-compile-initial-macro-environment)
    (setq byte-compile-function-environment nil)
    (setq byte-compile-unresolved-functions nil)
    (setq byte-compile-noruntime-functions nil)
    (setq byte-compile-tag-number 0)
    (setq byte-compile-output nil)
    (setq byte-compile-depth 0)
    (setq byte-compile-maxdepth 0)
    ;;(setq byte-code-vector nil)
    (setq byte-compile-current-form nil)
    (setq byte-compile-dest-file nil)
    (setq byte-compile-current-file nil)
    (setq byte-compile-current-group nil)
    (setq byte-compile-current-buffer nil)
    (setq byte-compile-read-position nil)
    (setq byte-compile-last-position nil)
    (setq byte-compile-last-warned-form nil)
    (setq byte-compile-last-logged-file nil)
    ;;(defvar bytecomp-outbuffer)
    ;;(defvar byte-code-meter)
    ;; Try compiling something ...
    (unless (file-exists-p web-autoload-cleanup-dummy-el)
      (let ((buf (find-file-noselect web-autoload-cleanup-dummy-el)))
        (with-current-buffer buf
          (insert ";; Dummy")
          (basic-save-buffer)
          (kill-buffer))))
    (byte-compile-file web-autoload-cleanup-dummy-el nil)))

(defvar web-autoload-compile-queue nil)
(defun web-autoload-byte-compile-file (file load)
  (if nil ;;(file-exists-p file)
      (byte-compile-file file load)
    (web-vcs-message-with-face 'web-vcs-gold "Add to compile queue (%S %s)" file load)
    (setq web-autoload-compile-queue (cons (cons file load)
                                           web-autoload-compile-queue))
    (if (< 1 (length web-autoload-compile-queue))
        (throw 'web-autoload-comp-restart t)
      (web-autoload-byte-compile-queue))))

;;(web-autoload-byte-compile-queue)
(defun web-autoload-byte-compile-queue ()
  (while web-autoload-compile-queue
    (when (catch 'web-autoload-comp-restart
            (if (not (web-autoload-byte-compile-file-1))
                t ;; Try again
              (setq web-autoload-compile-queue (cdr web-autoload-compile-queue))
              nil))
      ;; Clean up before restart
      (web-autoload-try-cleanup-after-failed-compile))
    ))

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
          nil ;;(when load (load elc-file))
        (when nil
          (condition-case err
              (progn
                (web-vcs-message-with-face 'font-lock-comment-face "Start batch byte compiling %S" file)
                (let ((emacs-exe (locate-file invocation-name (list invocation-directory) exec-suffixes))
                      (comp-buf (get-buffer-create "*Compile-Log*"))
                      (this-win (selected-window)))
                  (switch-to-buffer-other-window comp-buf)
                  (goto-char (point-max))
                  (select-window this-win)
                  (apply 'call-process emacs-exe nil comp-buf nil "-Q" "-batch" "-f" "batch-byte-compile" file nil))
                (web-vcs-message-with-face 'font-lock-comment-face "Ready batch byte compiling %S" file))
            (error
             (web-vcs-message-with-face
              'web-vcs-red "Error in batch byte compiling %S: %s" file (error-message-string err)))))
        (unless (file-exists-p elc-file)
          (condition-case err
              (progn
                (web-vcs-message-with-face 'font-lock-comment-face "Start byte compiling %S" file)
                ;;(when (ad-is-advised 'require) (ad-disable-advice 'require 'around 'web-autoload-ad-require))
                (let ((web-auto-load-skip-require-advice t)) (byte-compile-file file load))
                ;;(when (ad-is-advised 'require) (ad-enable-advice 'require 'around 'web-autoload-ad-require))
                (web-vcs-message-with-face 'font-lock-comment-face "Ready byte compiling %S" file))
            (error
             (web-vcs-message-with-face
              'web-vcs-red "Error in byte compiling %S: %s" file (error-message-string err))))
          )
        )
      ;; fix-me: Always return t on normal exits to tell to remove the
      ;; (possibly) compiled entry.
      (when (file-exists-p elc-file) t)))

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
      (if (or filename
              (and noerror
                   (or (not (boundp 'web-auto-load-skip-require-advice))
                       web-auto-load-skip-require-advice)))
          (progn
            (message "Doing nearly original require %s, because skipping" (ad-get-arg 0))
            ;; Can't ad-do-it because defadviced functions in load
            ;;(web-autoload-do-require feature filename noerror)
            ad-do-it
            )
        (let* ((auto-rec (assq feature web-autoload-require-list))
               (web-vcs      (nth 1 auto-rec))
               (base-url     (nth 2 auto-rec))
               (relative-url (nth 3 auto-rec))
               (base-dir     (nth 4 auto-rec)))
          (if (not auto-rec)
              ad-do-it
            (let* ((full-el      (concat (expand-file-name relative-url base-dir) ".el"))
                   (full-elc     (byte-compile-dest-file full-el)))
              (if (not auto-rec)
                  (progn
                    (message "Doing nearly original require %s, because no auto-rec" feature)
                    ;;(web-autoload-do-require feature filename noerror)
                    ;;(ad-set-arg 2 t)
                    ad-do-it
                    ;;(ad-set-arg 2 noerror)
                    )
                (web-vcs-message-with-face 'web-vcs-gold "Doing the really adviced require for %s" feature)
                ;; Check if already downloaded first
                (unless (file-exists-p full-el)
                  ;; Download and try again
                  (setq relative-url (concat relative-url ".el"))
                  (web-vcs-message-with-face 'font-lock-comment-face "Need to download feature %s (%S %S => %S)" feature base-url relative-url base-dir)
                  (catch 'command-level
                    (web-vcs-get-missing-matching-files web-vcs base-url base-dir relative-url))
                  (web-vcs-message-with-face 'font-lock-comment-face "After downloaded feature %s (%S %S => %S)" feature base-url relative-url base-dir))
                (unless (file-exists-p full-elc)
                  ;; Byte compile the downloaded file
                  (when web-autoload-autocompile
                    (web-autoload-byte-compile-file full-el t)))
                ;;(web-autoload-do-require feature filename noerror)
                ad-do-it
                ))))))))

(defun big-trace ()
  (setq trace-buffer "*Messages*")
  (trace-function-background 'byte-compile-form)
  (trace-function-background 'byte-compile-file-form)
  (trace-function-background 'byte-optimize-form)
  (trace-function-background 'byte-compile-normal-call)
  (trace-function-background 'byte-compile-cl-warn)
  (trace-function-background 'byte-compile-const-symbol-p)
  (trace-function-background 'byte-compile-warn)
  (trace-function-background 'byte-compile-warning-enabled-p)
  (trace-function-background 'byte-compile-callargs-warn)
  (trace-function-background 'byte-compile-splice-in-already-compiled-code)
  (trace-function-background 'byte-inline-lapcode)
  (trace-function-background 'byte-decompile-bytecode-1)
  )

;;(big-trace)

(provide 'web-autoload)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-autoload.el ends here
