;;; w32wds.el --- Windows Desktop Search integration
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
;; For more information see `w32wds-search'.
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

(eval-when-compile (require 'compile))
(require 'powershell-mode)
(require 'nxhtml-base)

(defvar w32wds-search-patt-hist nil)

(defvar w32wds-search-script (expand-file-name "etc/wds/DesktopSearch.rb" nxhtml-install-dir))
;; Fix-me: maybe. I am unable to get the ps1 version to work nicely
;; from within Emacs. It works but you can't have spaces in the script
;; file name, it is slower than the ruby version and there are more
;; chars not displayed correctly (since it works through cmd.exe
;; perhaps, but that is the only way to get it working currently,
;; i.e. Powershell 2.0, WinXP).
;;
;; Therefor I did not finish all details in this script.
;;
;;(setq w32wds-search-script (expand-file-name "etc/wds/DesktopSearch.ps1" nxhtml-install-dir))

;; (w32wds-search '("cullberg") '("c"))
;;;###autoload
(defun w32wds-search (search-patts file-patts params)
  "Search using Windows Search.
This searches all the content you have indexed there.

In interactive use you are prompted for a search string and a
single root directory.

The search string should consist of single word and phrases
\"enclosed like this\".  All words and phrases must match for a
file to match.

If the file is a text file it will be searched for all words and
phrases so you get direct links into it.

----
For non-interactive use SEARCH-PATTS and FILE-PATTS should be
list of strings.  In this case the strings are given as they are
to the SQL statements for searching Windows Search.

The strings in SEARCH-PATT should just be strings to match.  If
they contain spaces they are considered to be a sequence of
words, otherwise just single words.  All strings must match a
file for a match in that file.

The strings in FILE-PATTS are matched with the SQL keyword
'like'.  A '%' char is appended to each strings.  Any of this
strings should match.  This way you can easily search in
different root locations at once."
  (interactive
   (let* ((dir (read-directory-name "In directory tree: "))
          ;; Fix-me: split out the reading of search patterns.
          ;; (def-str (if (region-active-p)
          ;;              (concat "\""
          ;;                      (buffer-substring-no-properties (region-beginning) (region-end))
          ;;                      "\"")
          ;;            (or (let ((w (word-at-point)))
          ;;                  (when w (substring-no-properties w)))
          ;;                "")))
          (def-str (grep-tag-default))
          (str (read-string "Search patterns: " def-str 'w32wds-search-patt-hist))
          (item-patt (rx (or (and "\""
                                  (submatch (* (not (any "\""))))
                                  "\"")
                             (submatch (and word-start
                                            (+ (not space))
                                            word-end)))))
         (start 0)
         strs)
     (while (setq start (string-match item-patt str start))
       (let ((y (or (match-string 1 str)
                    (match-string 2 str))))
         (setq start (+ start (length y)))
         (setq strs (cons y strs))))

     ;; (setq dir (replace-regexp-in-string "/" "\\" dir t t))
     (list strs
           (list dir)
           "")))
  (w32wds-search-1 (list
                    "--root"   (mapconcat 'identity file-patts ",")
                    ;; "--locate" "grep"
                    "--query"  (mapconcat 'identity search-patts ","))))

;; (setq locate-make-command-line 'w32wds-locate-make-command-line)
;; (w32wds-locate-make-command-line "some.fil")
(defun w32wds-locate-make-command-line (search)
  (let* ((cmd (car (w32wds-make-command
                    (list
                     "--root"   default-directory
                     "--locate" "locate"
                     "--query"  search)))))
    cmd))

(defun w32wds-make-command (options)
  (let* ((script-ext (file-name-extension w32wds-search-script))
         (script-type (cond
                       ((string= "ps1" script-ext) 'powershell)
                       ((string= "rb"  script-ext) 'ruby)
                       (t 'unknown)))
         (command-list (append `(,(convert-standard-filename w32wds-search-script))
                               options)))
    (when (eq script-type 'ruby)
      (setq command-list (append '("ruby.exe") command-list)))
    (list command-list script-type)))

(defun w32wds-search-1 (options)
  (let* ((cmds (w32wds-make-command options))
         (cmd (car cmds))
         (script-type (cadr cmds))
         (default-directory (car (split-string (cadr (member "--root" options)) ",")))
         ;; Coding systems. To my surprise this seems to work for ruby at least!
         (process-coding-system-alist
          '((".*DesktopSearch.ps1.*" . utf-8)
            (".*powershell.exe.*" . utf-8)
            (".*ruby.exe" . utf-8)
            )))
    (unless (and (get 'compilation-start 'command-can-be-list)
               (not (eq script-type 'powershell)))
      ;; Fix-me: Or rather hope that my patch to compilation-start is
      ;; accepted soon...
      (setq cmd (mapconcat 'identity cmd " ")))
    (with-current-buffer (compilation-start cmd 'w32wds-mode)
      (visual-line-mode 1)
      (setq wrap-prefix "           "))
    ))

(defconst w32wds-error-regexp-alist
  '(("^\\(.+?\\)\\(:[ \t]*\\)\\([0-9]+\\)\\2"
     1 3)
    ("^\\(\\(.+?\\):\\([0-9]+\\):\\).*?\
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
    ("^File \\(.+\\) matches$" 1 nil nil 0 1)
    ;;("^File \\(.+\\) matches$" 1 nil nil 0 1)
    )
  "Regexp used to match search hits.  See `compilation-error-regexp-alist'.")

(defvar w32wds-mode-font-lock-keywords
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
      (2 compilation-error-face nil t))
     (w32wds-hit-marker)
     )
   "Additional things to highlight in w32wds mode.
This gets tacked on the end of the generated expressions.")

(defvar w32wds-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] 'w32wds-org-open-at-point)
    map))

(defun w32wds-hit-marker (bound)
  (let ((here (point)))
    (while (and (< (point) bound)
                (re-search-forward "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]" bound t))
      (let ((b0 (match-beginning 0))
            (e0 (match-end 0))
            (m1 (match-string 1))
            (b2 (match-beginning 2))
            (e2 (match-end 2)))
        (with-silent-modifications
          (put-text-property b0 b2 'invisible t)
          (put-text-property (- e0 1) (+ e0 1) 'invisible t)
          (put-text-property b2 e2 'help-echo m1)
          (put-text-property b2 e2 'keymap w32wds-link-keymap)
          (put-text-property b2 e2 'mouse-face 'highlight)
          (put-text-property b2 e2 'font-lock-face 'font-lock-function-name-face)
          )))
    (goto-char here)
    (while (and (< (point) bound)
                (re-search-forward "{{{\\(.*?\\)}}}" bound t))
      (let ((b0 (match-beginning 0))
            (e0 (match-end 0))
            (b1 (match-beginning 1))
            (e1 (match-end 1)))
        (with-silent-modifications
          (put-text-property b0 (- b1 0) 'invisible t)
          (put-text-property e1 (- e0 0) 'invisible t)
          (put-text-property b1 e1 'font-lock-face 'font-lock-keyword-face)
          )))
    nil))

(defun w32wds-org-open-at-point ()
  (interactive)
  (let* ((file (w32wds-find-filename))
         (full (expand-file-name file))
         (default-directory (file-name-directory full)))
    (org-open-at-point)))

(defvar w32wds-hit-face	compilation-info-face
  "Face name to use for search hits.")

(defun w32wds-find-filename ()
  (let ((here (point))
        (file-loc-patt "^File .* matches$"))
    (unless (re-search-backward file-loc-patt nil t)
      (error "Expected to find line matching %S above" file-loc-patt))
    (forward-char 12)
    (let ((file-msg (get-text-property (point) 'message)))
      (goto-char here)
      (caar (nth 2 (nth 0 file-msg))))))

(defun w32wds-next-error-function (n &optional reset)
  (let ((here (point)))
    (goto-char (point-at-bol))
    (if (not (looking-at "[a-z]:\\([0-9]+\\):"))
        (progn
          (goto-char here)
          (compilation-next-error-function n reset))
      (let ((line (string-to-number (match-string-no-properties 1)))
            (msg-pt (point))
            (msg (get-text-property (point) 'message))
            file)
        (setq compilation-current-error (point-marker))
        (setq file (w32wds-find-filename))
        (setcar (car (nth 2 (nth 0 msg))) file)
        (goto-char msg-pt)
        (let ((inhibit-read-only t))
          (put-text-property (point) (1+ (point)) 'message msg))
        (compilation-next-error-function n reset)
        ;;(goto-line line)
        ))))

(define-compilation-mode w32wds-mode "Search"
  "Mode for `w32wds-search' output."
  (setq next-error-function 'w32wds-next-error-function)
  (set (make-local-variable 'compilation-error-face) w32wds-hit-face)
  ;;(set (make-local-variable 'compilation-error-regexp-alist) w32wds-regexp-alist)
  ;;(set (make-local-variable 'compilation-process-setup-function) 'grep-process-setup)
  ;; (message "flkw=%S" compilation-mode-font-lock-keywords)
  )

(defun w32wds-add-powershell-kw ()
  (let ((kw `((,(cadr powershell-compilation-error-regexp-alist)
              (1 'compilation-error)
              (2 compilation-line-face nil t)
              (0
               (compilation-error-properties '1 2 nil nil nil '2 'nil)
               append))))
        )
    (font-lock-add-keywords 'w32wds-mode kw)))
(add-hook 'w32wds-mode-hook 'w32wds-add-powershell-kw)

(provide 'w32wds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w32wds.el ends here
