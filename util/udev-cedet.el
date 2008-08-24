;;; udev-cedet.el --- Get CEDET sources and set it up
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-22T13:06:48+0200 Fri
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

(defcustom udev-cedet-dir "c:/cedet/cedet"
  "Directory where to put CVS CEDET sources."
  :type 'directory
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
  (format "*Updating CEDET %s*" (car udev-this-step)))

(defvar udev-cedet-update-buffer nil)

(defun udev-cedet-update ()
  (interactive)
  (setq udev-cedet-update-buffer (get-buffer-create "*Update CEDET*"))
  (switch-to-buffer udev-cedet-update-buffer)
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (widen)
    (goto-char (point-max))
    (unless (= (point) (point-min)) (insert "\n\n"))
    (insert "Starting updating CEDET from development sources"))
  (setq udev-this-step udev-cedet-steps)
  (udev-call-this-step))

(defvar udev-cedet-fetch-buffer nil)

(defun udev-cedet-fetch ()
  (let* ((default-directory (file-name-as-directory udev-cedet-dir)) ;; fix-me: for emacs bug
         (cedet-root (expand-file-name ".." udev-cedet-dir)))
    (unless (file-directory-p cedet-root)
      (error "Directory %s does not exist" cedet-root))
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
            (let* ((default-directory (file-name-as-directory udev-cedet-dir))) ;; fix-me: for emacs bug
              (setq udev-cedet-diff-file (expand-file-name "../patches.diff"))
              (with-current-buffer
                  (compilation-start
                   (concat "cvs diff -b -u > " udev-cedet-diff-file)
                   'udev-cedet-compilation-mode
                   'udev-cedet-buffer-name)
                (setq udev-continue-on-error t)
                (current-buffer)))))))

(defvar udev-cedet-fetch-diff-buffer nil)

(defun udev-cedet-check-diff ()
  (when udev-cedet-fetch-diff-buffer
    (let ((buf (find-file-noselect udev-cedet-diff-file)))
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (if (search-forward "<<<<<<<" nil t)
            ;; Merge conflict
            (udev-call-next-step 1 nil)
          buf)))))

(defun udev-cedet-install ()
  (if nil
      (progn
        (load-file (expand-file-name "cedet-build.el" udev-cedet-dir))
        (cedet-build-in-default-emacs))
    (let* ((default-directory (file-name-as-directory udev-cedet-dir))) ;; fix-me: for emacs bug
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
