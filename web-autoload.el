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
  (unless (functionp fun)
    (let ((int (when interactive '(interactive))))
      (cond
       ((eq type 'macro)
        (setq type 'defmacro))
       (t
        (setq type 'defun)))
      (put fun 'web-autoload src)
      (eval
       `(web-autoload-1 ,fun ,src ,docstring ,int ,type)))))

(defun web-autoload-default-filename-element ()
  ;; Fix-me: el or elc?
  (expand-file-name "nxhtml-loaddefs.elc" nxhtml-install-dir))

(defcustom web-autoload-autocompile t
  "Byt compile downloaded files if t."
  :type 'boolean
  :group 'web-autoload)

;; Fix-me: change name
(defvar web-autoload-skip-require-advice nil)

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
                           (web-autoload-default-filename-element)))
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
           (let* (;;(vcs      (nth 0 ',src))
                  ;;(base-url (nth 1 ',src))
                  (rel-url  (nth 2 ',src))
                  ;;(base-dir (nth 3 ',src))
                  ;;(rel-url-el (concat rel-url ".el"))
                  ;;file-url
                  ;;dl-file
                  )
             ;;(unless (stringp base-url) (setq base-url (symbol-value base-url)))
             ;;(unless (stringp base-dir) (setq base-dir (symbol-value base-dir)))
             ;;(setq dl-file (expand-file-name rel-url-el base-dir))
             (web-vcs-message-with-face 'web-vcs-gold "web-autoload-1: BEG fun=%s" ',fun)
             ;; Fix-me: assume we can do require (instead of load, so
             ;; we do not have to defadvice load to).
             (unless (ad-is-advised 'require)
               (error "web-autoload-1: require is not advised"))
             (unless (ad-is-active 'require)
               (error "web-autoload-1: require advice is not active"))
             (when (catch 'web-autoload-comp-restart
                     (require (intern (file-name-nondirectory rel-url)))
                     nil)
               (web-autoload-byte-compile-queue))
             (when (equal (symbol-function ',fun) auto-fun)
               (error "Couldn't web autoload function %s" ',fun))
             (web-vcs-message-with-face 'web-vcs-gold "web-autoload-1: END fun=%s" ',fun)
             ))
         ;; Fix-me: Wrong place to do the cleanup! It must be done
         ;; after loading a file. All autoload in that file must be
         ;; deleted from the nxhtml-loaddefs entry.
         ;;
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
               (eval the-macro))))))))

;; Fix-me: Set up a byte compilation queue. Move function for byte compiling here.

(defvar web-autoload-cleanup-dummy-el
  (let* ((this-dir (file-name-directory (or load-file-name
                                            (when (boundp 'bytecomp-filename) bytecomp-filename)
                                            buffer-file-name))))
    (expand-file-name "temp-cleanup.el" this-dir)))

(defun web-autoload-try-cleanup-after-failed-compile (active-comp)
  (let* ((bc-input-buffer (get-buffer " *Compiler Input*"))
         (bc-outbuffer (get-buffer " *Compiler Output*"))
         ;;(active-comp (car web-autoload-compile-queue))
         (active-file (car active-comp))
         (active-elc (byte-compile-dest-file active-file)))
    ;; Delete bytecomp buffers
    (display-buffer "*Messages*")
    (web-vcs-message-with-face 'web-vcs-red "Trying to cleanup (%s %s %s)" bc-input-buffer bc-outbuffer active-elc)
    (when bc-input-buffer (kill-buffer bc-input-buffer))
    (when bc-outbuffer
      (kill-buffer bc-outbuffer)
      (setq bytecomp-outbuffer nil))
    ;; Delete half finished elc file
    (when (file-exists-p active-elc)
      (delete-file active-elc))
    ;; Delete load-history entry
    (when nil
      (setq load-history (cdr load-history)))
    ;; Try to reset some variables (just guesses)
    (when nil
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
      )
    ;; Try compiling something go get right state ...
    (when nil
      (unless (file-exists-p web-autoload-cleanup-dummy-el)
        (let ((buf (find-file-noselect web-autoload-cleanup-dummy-el)))
          (with-current-buffer buf
            (insert ";; Dummy")
            (basic-save-buffer)
            (kill-buffer))))
      (byte-compile-file web-autoload-cleanup-dummy-el nil))))

(defvar web-autoload-compile-queue nil)
(defun web-autoload-byte-compile-file (file load)
  (if nil ;;(file-exists-p file)
      (byte-compile-file file load)
    (let ((added-entry (cons file load)))
      (if (member added-entry web-autoload-compile-queue)
          (setq added-entry nil)
        (web-vcs-message-with-face 'web-vcs-gold "Add to compile queue (%S %s)" file load)
        (setq web-autoload-compile-queue (cons added-entry
                                               web-autoload-compile-queue)))
      (when added-entry
        (if web-autoload-byte-compile-queue-active
            (throw 'web-autoload-comp-restart t)
          (web-autoload-byte-compile-queue))))))

(defvar web-autoload-byte-compile-queue-active nil) ;; Dyn var
;;(web-autoload-byte-compile-queue)
(defun web-autoload-byte-compile-queue ()
  (let ((top-entry)
        (web-autoload-byte-compile-queue-active t))
    (while (and web-autoload-compile-queue
                (not (equal top-entry
                            (car web-autoload-compile-queue))))
      (setq top-entry (car web-autoload-compile-queue))
      (catch 'web-autoload-comp-restart
        (web-autoload-byte-compile-file-1)
        (setq web-autoload-compile-queue (cdr web-autoload-compile-queue))))))

(defcustom web-autoload-paranoid t
  "Be paranoid and break to check each file after download."
  :type 'boolean
  :group 'web-autoload)

(defun web-autoload-continue-no-stop ()
  "Continue web auto download.
This is used after inspecting downloaded elisp files.  Set
`web-autoload-paranoid' to nil before contiuning to avoid further
breaks to check downloaded files."
  (interactive)
  (setq web-autoload-paranoid nil)
  (web-autoload-continue))

(defun web-autoload-continue ()
  "Continue web auto download.
This is used after inspecting downloaded elisp files."
  (interactive)
  (if (< 0 (recursion-depth))
      (exit-recursive-edit)
    (web-autoload-byte-compile-queue)))

(defun web-autoload-byte-compile-file-1 ()
  "Compile and load FILE. Or just load."
  (let* ((compiled-it nil)
         (first-entry (car web-autoload-compile-queue))
         (el-file (car first-entry))
         (load (cdr first-entry))
         (elc-file (byte-compile-dest-file el-file))
         (need-compile (or (not (file-exists-p elc-file))
                           (file-newer-than-file-p el-file elc-file))))
    (if (not need-compile)
        nil ;;(when load (load elc-file))
      (web-autoload-do-eval-requires el-file)
      (when (catch 'web-autoload-comp-restart
              (condition-case err
                  (progn
                    (web-vcs-message-with-face 'font-lock-comment-face "Start byte compiling %S" el-file)
                    (web-vcs-message-with-face 'web-vcs-pink "Compiling QUEUE: %S" web-autoload-compile-queue)
                    ;;(when (ad-is-advised 'require) (ad-disable-advice 'require 'around 'web-autoload-ad-require))
                    ;; Fix-me: different byte-compile commands for different packages:
                    (let ((web-autoload-skip-require-advice t)) (nxhtml-byte-compile-file el-file load))
                    ;;(when (ad-is-advised 'require) (ad-enable-advice 'require 'around 'web-autoload-ad-require))
                    (web-vcs-message-with-face 'font-lock-comment-face "Ready byte compiling %S" el-file)
                    ;; Return nil to tell there are no known problems
                    (if (file-exists-p elc-file)
                        nil
                      (display-buffer "*Messages*")
                      (web-vcs-message-with-face
                       'web-vcs-red "Error: byte compiling did not produce %S" elc-file)
                      ;; Clean up before restart
                      (web-autoload-try-cleanup-after-failed-compile first-entry)
                      t))
                (error
                 (display-buffer "*Messages*")
                 (web-vcs-message-with-face
                  'web-vcs-red "Error in byte compiling %S: %s" el-file (error-message-string err))
                 ;; Clean up before restart
                 (web-autoload-try-cleanup-after-failed-compile first-entry)
                 t ;; error
                 )))
        (throw 'web-autoload-comp-restart t)
        ))))

(defun web-autoload-do-eval-requires (el-file)
  "Do eval-when-compile and eval-and-compile."
  ;;(message "web-autoload-do-eval-requires %S" el-file)
  (let ((old-buf (find-buffer-visiting el-file)))
    (with-current-buffer (or old-buf (find-file-noselect el-file))
      (let ((here (point))
            (web-autoload-require-skip-noerror-entries t))
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;;(message "web-autoload-do-eval-requires cb=%s" (current-buffer))
          (while (progn
                   (while (progn (skip-chars-forward " \t\n\^l")
                                 (looking-at ";"))
                     (forward-line 1))
                   (not (eobp)))
            (let ((form (read (current-buffer))))
              (when (memq (car form) '(eval-when-compile eval-and-compile))
                (web-vcs-message-with-face 'web-vcs-gold "eval %S" form)
                (eval form))
              )))
        (if old-buf (kill-buffer) (goto-char here))))))


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

(defvar web-autoload-require-skip-noerror-entries nil)

(defadvice require (around
                    web-autoload-ad-require)
  (let ((feature  (ad-get-arg 0))
        (filename (ad-get-arg 1))
        (noerror  (ad-get-arg 2)))
    (if (featurep feature)
        feature
      (if (or filename
              (and noerror
                   (or (not (boundp 'web-autoload-skip-require-advice))
                       web-autoload-skip-require-advice)))
          (progn
            (message "Doing nearly original require %s, because skipping" (ad-get-arg 0))
            ;; Can't ad-do-it because defadviced functions in load
            ;;(web-autoload-do-require feature filename noerror)
            ;;
            ;; Fix-me: Implement lazy loading here? Could it be done with while-no-input?
            ;;
            ;;(when (assq feature web-autoload-require-list) )
            ad-do-it)
        (unless (and noerror
                     web-autoload-require-skip-noerror-entries)
          (let* ((auto-rec (assq feature web-autoload-require-list))
                 (web-vcs      (nth 1 auto-rec))
                 (base-url     (nth 2 auto-rec))
                 (relative-url (nth 3 auto-rec))
                 (base-dir     (nth 4 auto-rec)))
            (if (not auto-rec)
                ad-do-it
              (let* ((full-el      (concat (expand-file-name relative-url base-dir) ".el"))
                     (full-elc     (byte-compile-dest-file full-el))
                     (our-buffer   (current-buffer)) ;; Need to come back here
                     (our-wcfg     (current-window-configuration)))
                (web-vcs-message-with-face 'web-vcs-gold "Doing the really adviced require for %s" feature)
                ;; Check if already downloaded first
                (unless (file-exists-p full-el)
                  ;; Download and try again
                  (setq relative-url (concat relative-url ".el"))
                  (web-vcs-message-with-face 'font-lock-comment-face "Need to download feature %s (%S %S => %S)" feature base-url relative-url base-dir)
                  (catch 'web-autoload-comp-restart
                    (web-vcs-get-missing-matching-files web-vcs base-url base-dir relative-url)))
                (set-buffer our-buffer) ;; Before we load..
                (when web-autoload-autocompile
                  (unless (file-exists-p full-elc)
                    ;; Byte compile the downloaded file
                    (web-autoload-byte-compile-file full-el t)))
                (web-vcs-message-with-face 'web-vcs-gold "Doing finally require for %s" feature)
                (set-buffer our-buffer) ;; ... and after we load
                (set-window-configuration our-wcfg)
                ad-do-it))))))))

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

(defvar web-autoload-require-list nil)

(defun web-autoload-require (feature web-vcs base-url relative-url base-dir)
  "Prepare to download file if necessary when `require' is called.
WEB-VCS BASE-URL RELATIVE-URL"
  (add-to-list 'web-autoload-require-list `(,feature ,web-vcs ,base-url ,relative-url ,base-dir))
  )

;;(big-trace)

(provide 'web-autoload)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-autoload.el ends here
