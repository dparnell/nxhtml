;;; sex-mode.el --- Shell EXecute mode / Send to EXternal program
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-06-01T18:41:50+0200 Sun
;; Version: 0.5
;; Last-Updated: 2008-07-20T03:38:58+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `w32-reg-iface'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Open urls belonging to other programs with those programs. To
;; enable this turn on the global minor mode `sex-mode'.
;;
;; If you for example open a .pdf file with C-x C-f it can be opened
;; by the .pdf application you have set your computer to use.  This
;; can be used in for example `org-mode' to have links to files that
;; Emacs itself does not handle.
;;
;; Note: Currently this is only useable on w32. Could someone please
;; complete the missing pieces for GNU/Linux?
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
;; published by the Free Software Foundation; either version 2, or
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


;; (defvar sex-w32-file-extensions
;;   (when (fboundp 'w32-shell-execute)
;;     (let ((ext nil))
;;       (mapc (lambda (key)
;;               (when (= (string-to-char key) ?.)
;;                 (setq ext (cons key ext))))
;;             (w32-reg-iface-enum-values "HKCR/"))
;;       ext)))

(require 'w32-reg-iface nil t)

(defgroup sex nil
  "Customization group for `sex-mode'.")

;;(setq sex-w32-use-registry t)
;;(setq sex-w32-use-registry nil)
(defcustom sex-w32-use-registry t
    "Use file types from MS Windows registry.
See `sex-w32-ftypes' for what values are read.

If non-nil send all files except those associated with
`sex-w32-emacs-ftype' or prevented by `sex-open-alist' to the
shell.

Only used on w32."
    :type 'boolean
    :group 'sex)

(defcustom sex-w32-emacs-ftype "EmacsFile"
    "File type for .el files.
This is what

  assoc .el

in a command shell returns. The default value is the value
installed by Emacs+EmacsW32.

This value is used when checking the MS Windows registry, ie if
`sex-w32-use-registry' is non-nil.

Only used on w32."
    :type 'string
    :group 'sex)

;;(setq sex-handle-urls t)
(defcustom sex-handle-urls nil
  "When non-nil `sex-mode' also handles urls.
Turn on `url-handler-mode' when turning on `sex-mode' if this is
non-nil.  Open urls in a web browser."
  :type 'boolean
  :group 'sex)

(defcustom sex-open-alist
  '(
    ;; These are normally not handled by Emacs
    ("\\.pdf\\'" t)
    ("\\.doc\\'" t)
    ("\\.dot\\'" t)
    ("\\.7z\\'" t)
    ("\\.bz2\\'" t)
    ("\\.bzip2\\'" t)
    ("\\.avi\\'" t)
    ("\\.cpl\\'" t)
    ("\\.cur\\'" t)
    ;; fix-me: continue ...

    ;; These can be handled by Emacs
    ("\\.png\\'" nil)
    ("\\.gif\\'" nil)
    ;; All scripts and exectutables should be opened by Emacs
    ("\\.exe\\'" nil)
    ("\\.cmd\\'" nil)
    ("\\.bat\\'" nil)
    ("\\.vbs\\'" nil)
    ("\\.pl\\'" nil)
    ("\\.pm\\'" nil)
    ("\\.py\\'" nil)
    ;; fix-me: continue ...
    )
  "Alist of file name patterns to handle.
Entries in the list have the form

  (FILE-REGEXP OPEN-BY-SHELL)

where FILE-REGEXP is a regular expression to match a file name.
OPEN-BY-SHELL is a boolean.

When `sex-mode' is on the rules for deciding if a file should be
opened by the shell is:

 If file name matches in this list then
 - If OPEN-BY-SHELL is nil always open inside Emacs.
 - If OPEN-BY-SHELL is non-nil then open the file with the shell.

 Otherwise (if file name does not matches in this list) then
 - If `sex-w32-use-registry is nil open inside Emacs.
 - If `sex-w32-use-registry is non-nil then see this variable.

When opening a file with the shell a dummy buffer is created in
Emacs in `sex-file-mode' and an external program is called to
handle the file. How this dummy buffer is handled is governed by
`sex-keep-dummy-buffer'."
  :type '(repeat (list
                  (regexp :tag "Filename regexp")
                  (boolean :tag "Open with external program")))
  :group 'sex)

;; (setq sex-keep-dummy-buffer nil)
;; (setq sex-keep-dummy-buffer 'visible)
;; (setq sex-keep-dummy-buffer 'burried)
(defcustom sex-keep-dummy-buffer nil
  "Keep dummy buffer after opening file.
See `sex-open-alist'."
  :type '(choice (const :tag "Visible" visible)
                 (const :tag "Burried" burried)
                 (const :tag "Do not keep it" nil))
  :group 'sex)

(defcustom sex-reopen-on-buffer-entry t
  "If non-nil send file to shell again on buffer entry."
  :type 'boolean
  :group 'sex)

(defun sex-post-command ()
  "Run post command in `sex-file-mode' buffers.
If `sex-reopen-on-buffer-entry' is non-nil then send the buffer
file to system again."
  (when sex-reopen-on-buffer-entry
    (if (and (boundp 'url-handler-regexp)
             (string-match url-handler-regexp buffer-file-name))
        (sex-browse-url buffer-file-name)
      (sex-handle-by-external (buffer-file-name)))
    (bury-buffer)))

(defvar sex-w32-ftypes '(("el" emacs-file)
                         ("elc" emacs-file))
    "File types cache for registry values.
File types are fetched from registry like this:

  \(w32-reg-iface-read-value \"HKCR/.doc/\")
      => (\"OpenOffice.org.doc\" . \"REG_SZ\")

In this list (\"el\" \"OpenOffice.org.doc\") is cached.

Only used on w32.")

(defun sex-browse-url (url)
  "Ask a web browser to open URL."
  (condition-case err
      (list (browse-url url) "Opened URL in web browser")
    (error (list nil (error-message-string err)))))

(defun sex-url-insert-file-contents (url &optional visit beg end replace)
  (sex-generic-insert-file-contents
   'sex-browse-url
   (concat "This dummy buffer is used just for opening a URL.\n"
           "To open the URL again click here:\n\n  ")
   (concat "Tried to open URL in web browser, "
           "but it failed with message\n\n  ")
   url visit beg end replace))

(defun sex-file-insert-file-contents (url &optional visit beg end replace)
  (sex-generic-insert-file-contents
   'sex-handle-by-external
   (concat "This dummy buffer is used just for opening a file.\n"
           "The file itself was sent to system for opening.\n\n"
           "To open the file again click here:\n\n  ")
   (concat "Tried to send file"
           " to system but it failed with message\n\n  ")
   url visit beg end replace))

(defun sex-write-file-function ()
  (set-buffer-modified-p nil)
  (error "Can't write this to file, it is just a dummy buffer"))

(defun sex-generic-insert-file-contents (insert-fun
                                         success-header
                                         fail-header
                                         url &optional visit beg end replace)
  (let ((window-config (current-window-configuration)))
    (unless (= 0 (buffer-size))
      (error "Buffer must be empty"))
    (set (make-local-variable 'write-file-functions) '(sex-write-file-function))
    (let* ((name url)
           ;;(result (sex-browse-url name))
           (result (funcall insert-fun name))
           (success (nth 0 result))
           (msg     (nth 1 result)))
      (setq buffer-file-name name)
      (if success
          (progn
            (insert success-header)
            (sex-setup-restore-window-config)
            (message "%s" msg))
        (insert (propertize "Error: " 'face 'font-lock-warning-face)
                fail-header msg
                "\n\nTo try again click here:\n\n  "))
      (save-excursion
        (insert-text-button
         buffer-file-name
         'action (lambda (button)
                   (sex-browse-url buffer-file-name)))))))

(defun sex-file-handler (operation &rest args)
  "Handler for `insert-file-contents'."
  (let ((done nil)
        (ftype 'emacs-file))
    ;; Always open files inside Emacs if the file opening request came
    ;; through Emacs client. Here is a primitive test if we are called
    ;; from outside, client-record is bound in `server-visit-files'
    ;; ...
    (when (not (boundp 'client-record))
      (let* ((filename (car args))
             fileext
             rec
             (sex-open-alist-handling
              (catch 'sex-open-special
                (dolist (srec sex-open-alist)
                  (when (string-match (car srec) filename)
                    ;;(message "special srec=%s" srec)
                    (throw 'sex-open-special (if (nth 1 srec)
                                                 'shell
                                               'emacs-file)))))))
        (if sex-open-alist-handling
            (setq ftype sex-open-alist-handling)
          (when (and (fboundp 'w32-shell-execute)
                     (featurep 'w32-reg-iface)
                     sex-w32-use-registry)
            (setq fileext  (file-name-extension filename))
            (setq rec (assoc fileext sex-w32-ftypes))
            (unless rec
              (setq rec
                    (or (let* ((regrec (w32-reg-iface-read-value
                                        (concat "HKCR/." fileext "/")))
                               ;; (w32-reg-iface-read-value "HKCR/.none12/")
                               ;; (w32-reg-iface-read-value "HKCR/.doc/")
                               ;; (w32-reg-iface-read-value "HKCR/.el/")
                               ;; (w32-reg-iface-read-value "HKCR/.elc/")
                               ;; (w32-reg-iface-read-value "HKCR/.txt/")
                               (regftype (car regrec)))
                          (when regftype
                            (when (string= regftype sex-w32-emacs-ftype)
                              (setq regftype 'emacs-file))
                            (list fileext regftype)))
                        (list fileext 'emacs-file)))
              (add-to-list 'sex-w32-ftypes rec))
            (setq ftype (nth 1 rec))))))
    ;;(message "filename=%s; ftype=%s" (car args) ftype)
    (unless (eq ftype 'emacs-file)
      ;;(message "using sex-file-insert-file-contents for %s" filename)
      (apply 'sex-file-insert-file-contents args)
      (setq done t))
    ;; Handle any operation we don't know about.
    (unless done
      ;;(message "operation=%s, args=%s" operation args)
      (let ((inhibit-file-name-handlers
             (cons 'sex-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args)))))
;; Note: Because of a bug in Emacs we must restrict the use of this
;; file handler to only 'insert-file-contents. (We should of course
;; anyway do that.)
(put 'sex-file-handler 'operations '(insert-file-contents))

(defun sex-setup-restore-window-config ()
  (when (not (eq sex-keep-dummy-buffer 'visible))
    (run-with-idle-timer 0 nil
                         'sex-restore-window-config
                         (selected-frame)
                         window-config
                         (unless sex-keep-dummy-buffer
                           (current-buffer)))))

(defun sex-restore-window-config (frame win-config buffer)
  (with-selected-frame frame
    (set-window-configuration win-config))
  (when buffer (kill-buffer buffer)))

(defun sex-handle-by-external (&optional file)
  "Give file FILE to external program.
Return a list:

  (SUCCESS MESSAGE)

where SUCCESS is non-nil if operation succeeded and MESSAGE is an
informational message."
  (unless file (setq file buffer-file-name))
  (cond ((fboundp 'w32-shell-execute)
         (condition-case err
             (progn
               (w32-shell-execute "open" (convert-standard-filename file))
               (list t (concat "Sent file " file " to system")))
           (error
            (list nil (error-message-string err)))))
        (t
         (error "Don't know how to handle the file on your OS yet."))))

(define-derived-mode sex-file-mode nil
  "External"
  "Mode for files opened in external programs."
  (add-hook 'post-command-hook 'sex-post-command nil t)
  (set-keymap-parent (current-local-map) button-buffer-map)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))


(defvar sex-old-url-insert-file-contents nil)
(defvar sex-old-url-handler-mode nil)

(define-minor-mode sex-mode
  "Open certain files in external programs.
Which files to open this way can be choosen by customizing
`sex-open-alist' and and MS Windows also `sex-w32-use-registry'.

This affects all functions that opens files, like `find-file',
`find-file-noselect' etc.

However it does not affect files opened through Emacs client.

On MS Windows `w32-shell-execute' is called to open files in an
external application. Be aware that this may run scripts if the
script file extension is not blocked in `sex-open-alist'.

\(MS Windows is the only platform were this works yet. Additions
for other platforms are most welcome!)

Urls can also be handled, see `sex-handle-urls'."
nil
  :group 'sex
  :global t
  ;; fix-me: better list handling
  (if sex-mode
      (progn
        (when sex-w32-use-registry
          (add-to-list 'file-name-handler-alist (cons "\\.[a-z]+\\'" 'sex-file-handler) t))
        (dolist (rec sex-open-alist)
          (let ((patt (nth 0 rec))
                (on   (nth 1 rec)))
            (when on
              (add-to-list 'auto-mode-alist (cons patt 'sex-file-mode))
              (unless sex-w32-use-registry
                (add-to-list 'file-name-handler-alist (cons patt 'sex-file-handler) t)))))
        (setq sex-old-url-insert-file-contents (get 'insert-file-contents 'url-file-handlers))
        (setq sex-old-url-handler-mode url-handler-mode)
        (when sex-handle-urls
          ;;(message "req url, before")
          (require 'url-handlers)
          ;;(message "req url, after")
          (put 'insert-file-contents 'url-file-handlers 'sex-url-insert-file-contents)
          (unless url-handler-mode
            (url-handler-mode 1)
            ;;(message "after url-handler-mode 1")
            )))
    ;; Remove from the lists:
    (setq auto-mode-alist
          (delete (cons "\\.[a-z]+\\'" 'sex-file-mode) auto-mode-alist))
    (setq file-name-handler-alist
          (delete (cons "\\.[a-z]+\\'" 'sex-file-handler)
                  file-name-handler-alist))
    (dolist (rec sex-open-alist)
      (let ((patt (nth 0 rec)))
        (setq auto-mode-alist
              (delete (cons patt 'sex-file-mode) auto-mode-alist))
        (setq file-name-handler-alist
              (delete (cons patt 'sex-file-handler)
                      file-name-handler-alist))))
    (put 'insert-file-contents 'url-file-handlers sex-old-url-insert-file-contents)
    (unless sex-old-url-handler-mode (url-handler-mode 0))))

(defmacro with-sex (open-alist &rest body)
  "Run BODY with `sex-mode' on.
OPEN-ALIST replaces `sex-open-alist', or if it is t, use the
current value."
  (declare (indent 1) (debug t))
  `(let ((old-sex-mode sex-mode)
         (sex-open-alist (if (eq ,open-alist t)
                             sex-open-alist
                           ,open-alist)))
     (unless sex-mode (sex-mode 1))
     ,@body
     (unless old-sex-mode (sex-mode -1))))

;; (with-sex t (find-file "c:/emacs-lisp/gimp-mode-v1.40/gimpmode.pdf"))
;; (with-sex nil (find-file "c:/emacs-lisp/gimp-mode-v1.40/gimpmode.pdf"))

(provide 'sex-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sex-mode.el ends here
