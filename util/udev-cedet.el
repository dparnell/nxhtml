;;; udev-cedet.el --- Get CEDET sources and set it up
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-22
(defconst udev-cedet:version "0.2") ;; Version:
;; Last-Updated: 2008-08-24T18:19:15+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl', `udev'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Fetch and install CEDET from the devel sources.
;;
;; See `udev-cedet-update' for more information.
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

(require 'udev)

(defgroup udev-cedet nil
  "Customization group for udev-cedet."
  :group 'nxhtml)

(defcustom udev-cedet-dir "~/cedet-cvs/"
  "Directory where to put CVS CEDET sources."
  :type 'directory
  :group 'udev-cedet)

(defcustom udev-cedet-load-cedet nil
  "To load or not to load CEDET..."
  :type '(choice (const :tag "Don't load CEDET" nil)
                 (set :tag "Choose what to load"
                  (const :tag "EDE Project Management" ede)
                  (radio :tag "Choose parsing and completion features"
                   (const :tag "Minimum features (database+idle reparse)" min-parse)
                   (const :tag "Semantic navigator etc" code-helpers)
                   (const :tag "Intellisense etc" gaudy-code-helpers))
                  (const srecode)
                  )
                 (const :tag "Load whole CEDET" t))
  :set (lambda (sym val)
         (set-default sym val)
         (when val
           (let ((cedet-el (expand-file-name "cedet/common/cedet.el" udev-cedet-dir))
                 loaded)
             (unless (featurep 'cedet)
               (when (file-exists-p cedet-el)
                 (condition-case-no-debug err
                     (load-file cedet-el)
                   (error (message "%s" err))))
               (unless (featurep 'cedet)
                 (when (y-or-n-p "Could not load CEDET, update from dev sources?")
                   (udev-cedet-update)
                   (load-file cedet-el))
                 ))
             (when (featurep 'cedet)
               (let ((use-ede
                      (or (eq val t)
                          (memq 'ede val)))
                     (use-min-parse
                      (or (eq val t)
                          (memq 'min-parse val)))
                     (use-code-helpers
                      (or (eq val t)
                          (memq 'code-helpers val)))
                     (use-gaudy-code-helpers
                      (or (eq val t)
                          (memq 'gaudy-code-helpers val)))
                     )
                 (global-ede-mode (if use-ede 1 -1))
                 (when use-min-parse
                   (semantic-load-enable-minimum-features))
                 (when use-code-helpers
                   (semantic-load-enable-code-helpers))
                 (when use-gaudy-code-helpers
                   (semantic-load-enable-gaudy-code-helpers))
                 )))))
  :group 'udev-cedet)

(defun udev-cedet-fontify-marker (limit)
  (message "here 1 %s-%s" (point) limit)
  (when (= (point) (line-beginning-position))
    (when (eq (char-after) ?\ )
      (message "here 2")
      (put-text-property (point) (1+ (point))
                         'face 'highlight)
      )))

(define-derived-mode udev-cedet-compilation-mode compilation-mode
  "CVS command"
  "For cvs command output."
  ;;(font-lock-add-keywords nil '((udev-cedet-fontify-marker)))
  )

(defvar udev-cedet-steps
  '(udev-cedet-fetch
    udev-cedet-maybe-fetch-diff
    udev-cedet-check-diff
    udev-cedet-install
    ))

(defun udev-cedet-buffer-name (mode)
  (format "*Updating CEDET %s*"
          (udev-this-step udev-cedet-update-buffer)))

(defvar udev-cedet-update-buffer nil)

(defun udev-cedet-update ()
  "Fetch and install CEDET from the devel sources.
To determine where to store the sources and how to start CEDET
see `udev-cedet-dir' and `udev-cedet-load-cedet'."
  (interactive)
  (setq udev-cedet-update-buffer (get-buffer-create "*Update CEDET*"))
  (switch-to-buffer udev-cedet-update-buffer)
  (let ((inhibit-read-only t))
    (widen)
    (goto-char (point-max))
    (unless (= (point) (point-min)) (insert "\n\n"))
    (insert "Starting updating CEDET from development sources"))
  (udev-call-first-step udev-cedet-update-buffer udev-cedet-steps))

(defvar udev-cedet-fetch-buffer nil)

(defun udev-cedet-fetch ()
  (let* ((default-directory (file-name-as-directory udev-cedet-dir)) ;; fix-me: for emacs bug
         )
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (with-current-buffer
        (compilation-start
         "cvs -z3 -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet co -P cedet"
         'udev-cedet-compilation-mode
         'udev-cedet-buffer-name)
      (setq udev-cedet-fetch-buffer (current-buffer)))))

(defvar udev-cedet-diff-file nil)

(defun udev-cedet-maybe-fetch-diff ()
  (let ((must-fetch-diff nil))
    (if (not udev-cedet-fetch-buffer)
        (setq must-fetch-diff t)
      ;; Fix-me: This search is not useful. There are no lines
      ;; beginning with "M ". Why?
      (with-current-buffer udev-cedet-fetch-buffer
        (widen)
        (goto-char (point-min))
        (let ((lnum 1)
              (pos 1))
          (while (and (< lnum 200)
                      (< (setq pos (line-beginning-position 1))
                         (point-max)))
            (setq lnum (1+ lnum))
            (when (string= (buffer-substring-no-properties
                            pos (+ pos 2))
                           "M ")
              (setq must-fetch-diff t))))))
    (setq must-fetch-diff t)
    (setq udev-cedet-fetch-diff-buffer
          (when must-fetch-diff
            (let* ((default-directory (file-name-as-directory
                                       (expand-file-name "cedet"
                                                         udev-cedet-dir))))
              ;;(setq default-directory (file-name-as-directory udev-cedet-dir))
              (setq udev-cedet-diff-file (expand-file-name "../patches.diff"))
              (with-current-buffer
                  (compilation-start
                   (concat "cvs diff -b -u > " (shell-quote-argument udev-cedet-diff-file))
                   'udev-cedet-compilation-mode
                   'udev-cedet-buffer-name)
                (setq udev-continue-on-error-function 'udev-cvs-diff-continue)
                (current-buffer)))))))

(defun udev-cvs-diff-continue (cvs-diff-buffer)
  (with-current-buffer cvs-diff-buffer
    (let ((here (point))
          (ret t))
      (goto-char (point-min))
      ;; From cvs co command:
      ;; rcsmerge: warning: conflicts during merge
      ;; cvs checkout: conflicts found in emacs/lisp/startup.el
      (when (search-forward "cvs [diff aborted]" nil t)
        (setq ret nil))
      (when (search-forward "merge conflict" nil t)
        (setq ret nil))
      (goto-char here)
      ret)))

(defvar udev-cedet-fetch-diff-buffer nil)

(defun udev-cedet-check-diff ()
  (when udev-cedet-fetch-diff-buffer
    (let ((buf (find-file-noselect udev-cedet-diff-file)))
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (if (search-forward "<<<<<<<" nil t)
            ;; Merge conflict
            (udev-call-next-step udev-cedet-update-buffer 1 nil)
          buf)))))

(defun udev-cedet-install ()
  (if nil
      (progn
        (load-file (expand-file-name "cedet/cedet-build.el" udev-cedet-dir))
        (cedet-build-in-default-emacs))
    (let* ((default-directory (file-name-as-directory
                               (expand-file-name "cedet" udev-cedet-dir))))
      (with-current-buffer
          (compilation-start
           ;;"cvs -z3 -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet co -P cedet"
           "emacs -Q -batch -l cedet-build.el -f cedet-build"
           'udev-cedet-compilation-mode
           'udev-cedet-buffer-name)
        (current-buffer)))))

;;(setq compilation-scroll-output t)
;;(add-to-list 'compilation-error-regexp-alist 'cvs)
;;(setq compilation-error-regexp-alist (delq 'cvs compilation-error-regexp-alist))

(provide 'udev-cedet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev-cedet.el ends here
