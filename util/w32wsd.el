;;; w32wsd.el --- Windows Desktop Search integration
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-21 Tue
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
;; Integration with Windows Search.
;; For more information see `w32wsd-search'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
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

(defvar w32wsd-search-patt-hist nil)

(defvar w32wsd-search-script "DesktopSearch.ps1")

;; (w32wsd-search '("cullberg") '("c"))
;;;###autoload
(defun w32wsd-search (search-patts file-patts)
  "Search using Windows Search.
This searches all the content you have indexed there.

In interactive use you are prompted for a search strings and a
root directory.
"
  (interactive
   (let ((dir (read-directory-name "In directory tree: "))
         (srch (read-string "Search patterns: " nil 'w32wsd-search-patt-hist)))
     ;; (setq dir (replace-regexp-in-string "/" "\\" dir t t))
     (list (list srch)
           (list dir))))
  (let ((command (concat "DesktopSearch.ps1"
                         " "
                         (mapconcat 'identity file-patts ", ")
                         " "
                         (mapconcat 'identity search-patts ", ")))
        (dir default-directory))
    (let ((default-directory dir)
          ;; Fix-me: coding system
          (process-coding-system-alist
           '(
             (".*DesktopSearch.ps1.*" . utf-8)
             (".*powershell.exe.*" . utf-8)
             )))
      (compilation-start command 'w32wsd-mode)
      )))

(defconst w32wsd-error-regexp-alist
  '(("^ \\(.+?\\)\\(:[ \t]*\\)\\([0-9]+\\)\\2"
     1 3)
    ("^ y\\(\\(.+?\\):\\([0-9]+\\):\\).*?\
\\(\033\\[01;31m\\(?:\033\\[K\\)?\\)\\(.*?\\)\\(\033\\[[0-9]*m\\)"
     2 3
     ;; Calculate column positions (beg . end) of first grep match on a line
     ((lambda ()
	(setq compilation-error-screen-columns nil)
        (- (match-beginning 4) (match-end 1)))
      .
      (lambda () (- (match-end 5) (match-end 1)
		    (- (match-end 4) (match-beginning 4)))))
     nil 1)
    ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1)
    ("^Text file \\(.+\\) matches:$" 1 nil nil 0 1)
    )
  "Regexp used to match search hits.  See `compilation-error-regexp-alist'.")

(defvar w32wsd-mode-font-lock-keywords
   '(;; configure output lines.
     ;; ("^[Cc]hecking \\(?:[Ff]or \\|[Ii]f \\|[Ww]hether \\(?:to \\)?\\)?\\(.+\\)\\.\\.\\. *\\(?:(cached) *\\)?\\(\\(yes\\(?: .+\\)?\\)\\|no\\|\\(.*\\)\\)$"
     ;;  (1 font-lock-variable-name-face)
     ;;  (2 (compilation-face '(4 . 3))))
     ;; Command output lines.  Recognize `make[n]:' lines too.
     ;; ("^\\([[:alnum:]_/.+-]+\\)\\(\\[\\([0-9]+\\)\\]\\)?[ \t]*:"
     ;;  (1 font-lock-function-name-face) (3 compilation-line-face nil t))
     ;; (" --?o\\(?:utfile\\|utput\\)?[= ]?\\(\\S +\\)" . 1)
     ("^\\(Search finished\\).*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face))
     ("^Compilation \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 compilation-error-face)
      (2 compilation-error-face nil t)))
   "Additional things to highlight in w32wsd mode.
This gets tacked on the end of the generated expressions.")

(defvar w32wsd-hit-face	compilation-info-face
  "Face name to use for search hits.")

(defun w32wsd-next-error-function (n &optional reset)
  (let ((here (point)))
    (goto-char (point-at-bol))
    (if (not (looking-at "  :\\([0-9]+\\):"))
        (progn
          (goto-char here)
          (compilation-next-error-function n reset))
      (let ((line (string-to-number (match-string-no-properties 1)))
            (msg-pt (point))
            (msg (get-text-property (point) 'message))
            file-msg
            file)
        (setq compilation-current-error (point-marker))
        (if (not (re-search-backward "Text file " nil t))
            (error "Expected to find line beginning with 'Text file' above")
          (forward-char 12)
          (setq file-msg (get-text-property (point) 'message))
          (setq file (caar (nth 2 (nth 0 file-msg))))
          (setcar (car (nth 2 (nth 0 msg))) file)
          (goto-char msg-pt)
          (let ((inhibit-read-only t))
            (put-text-property (point) (1+ (point)) 'message msg))
          (compilation-next-error-function n reset)
          ;;(goto-line line)
          )))))

(define-compilation-mode w32wsd-mode "Search"
  "Mode for `w32wsd-search' output."
  (setq next-error-function 'w32wsd-next-error-function)
  (set (make-local-variable 'compilation-error-face) w32wsd-hit-face)
  ;;(set (make-local-variable 'compilation-error-regexp-alist) w32wsd-regexp-alist)
  ;;(set (make-local-variable 'compilation-process-setup-function) 'grep-process-setup)
  (message "flkw=%S" compilation-mode-font-lock-keywords)
  )

(defun w32wsd-add-powershell-kw ()
  (require 'powershell-mode)
  (let ((kw `((,(cadr powershell-compilation-error-regexp-alist)
              (1 compilation-error-face)
              (2 compilation-line-face nil t)
              (0
               (compilation-error-properties '1 2 nil nil nil '2 'nil)
               append))))
        )
    (font-lock-add-keywords 'w32wsd-mode kw)))
(add-hook 'w32wsd-mode-hook 'w32wsd-add-powershell-kw)

(provide 'w32wsd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w32wsd.el ends here
