;;; idxsearch.el --- Windows Desktop Search integration
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-21 Tue
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:  matching
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `comint', `compile', `flymake',
;;   `flymake-css', `flymake-java-1', `flymake-js', `nxhtml-base',
;;   `powershell-mode', `ring', `tool-bar', `warnings'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Integration with Windows Search.
;; For more information see `idxsearch'.
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

;; Fix-me: rename to for example idxsearch.el
(eval-when-compile (require 'compile))
(eval-when-compile (require 'grep))
(eval-when-compile (require 'org))
(require 'powershell-mode)
(require 'nxhtml-base)
;; Fix-me: The byte compiler should not complain about these:
(declare-function orgstruct-mode "orgstruct-mode")
(declare-function outline-minor-mode "outline")

(defvar idxsearch-patt-hist nil)

(defvar idxsearch-search-script (expand-file-name "etc/wds/idxsearch.rb" nxhtml-install-dir))
;; Fix-me: maybe. I am unable to get the ps1 version to work nicely
;; from within Emacs. It works but you can't have spaces in the script
;; file name, it is slower than the ruby version and there are more
;; chars not displayed correctly (since it works through cmd.exe
;; perhaps, but that is the only way to get it working currently,
;; i.e. Powershell 2.0, WinXP).
;;
;; Therefor I did not finish all details in the ps1 script and it is
;; currently not usable with idxsearch.el. (Though it should be easy
;; to fix, it is just output formatting that differs.)
;;
;;(setq idxsearch-search-script (expand-file-name "etc/wds/idxsearch.ps1" nxhtml-install-dir))

;; (idxsearch '("cullberg") '("c:/") nil)
;;;###autoload
(defun idxsearch (search-patts file-patts params)
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

When called from elisp SEARCH-PATTS and FILE-PATTS should be list
of strings.  In this case the strings are given as they are to
the SQL statements for searching Windows Search.

The strings in SEARCH-PATT should just be strings to match.  If
they contain spaces they are considered to be a sequence of
words, otherwise just single words.  All strings must match a
file for a match in that file.

The strings in FILE-PATTS are matched with the SQL keyword
'like'.  A '%' char is appended to each strings.  Any of this
strings should match.  This way you can easily search in
different root locations at once."

;; Fix-me: option for matching long lines with all patterns, instead
;; of any.
  (interactive
   ;; Fix-me: Extract this and use as a loop for new queries. Allow
   ;; things like TAB cycle visibility in the output buffer. Add a
   ;; "reenter query command".
   (let* ((dir (read-directory-name "Indexed search in directory tree: "))
          ;; Fix-me: split out the reading of search patterns.
          ;; (def-str (if (region-active-p)
          ;;              (concat "\""
          ;;                      (buffer-substring-no-properties (region-beginning) (region-end))
          ;;                      "\"")
          ;;            (or (let ((w (word-at-point)))
          ;;                  (when w (substring-no-properties w)))
          ;;                "")))
          (def-str (grep-tag-default))
          (str (read-string "Search patterns: " def-str 'idxsearch-patt-hist))
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
  (idxsearch-1 (list
                    "--root"   (mapconcat 'identity file-patts ",")
                    ;; "--locate" "grep"
                    "--query"  (mapconcat 'identity search-patts ","))))

;; (setq locate-make-command-line 'idxsearch-locate-make-command-line)
;; (idxsearch-locate-make-command-line "some.fil")
(defun idxsearch-locate-make-command-line (search)
  (let* ((cmd (car (idxsearch-make-command
                    (list
                     "--root"   default-directory
                     "--locate" "locate"
                     "--query"  search)))))
    cmd))

(defun idxsearch-make-command (options)
  (let* ((script-ext (file-name-extension idxsearch-search-script))
         (script-type (cond
                       ((string= "ps1" script-ext) 'powershell)
                       ((string= "rb"  script-ext) 'ruby)
                       (t 'unknown)))
         (command-list (append `(,(convert-standard-filename idxsearch-search-script))
                               options)))
    (when (eq script-type 'ruby)
      (setq command-list (append '("ruby.exe") command-list)))
    (list command-list script-type)))

(defun idxsearch-1 (options)
  (let* ((cmds (idxsearch-make-command options))
         (cmd (car cmds))
         (script-type (cadr cmds))
         (default-directory (car (split-string (cadr (member "--root" options)) ",")))
         ;; Coding systems. To my surprise this seems to work for ruby
         ;; at least!
         (process-coding-system-alist
          '((".*idxsearch.ps1.*" . utf-8)
            (".*powershell.exe.*" . utf-8)
            (".*ruby.exe" . utf-8)
            )))
    (unless (and (get 'compilation-start 'command-can-be-list)
               (not (eq script-type 'powershell)))
      ;; Fix-me: Or rather hope that my patch to compilation-start is
      ;; accepted soon...
      (setq cmd (mapconcat 'identity cmd " ")))
    (with-current-buffer (compilation-start cmd 'idxsearch-mode)
      (visual-line-mode 1)
      (setq wrap-prefix "           ")
      (outline-minor-mode)
      ;; Fix-me: This prevents tab from beeing used outside header
      ;; lines, otherwise it is very nice. Sigh. Try to make Carsten
      ;; change this.

      ;; fix-me: Maybe add some highlighting to show that there headerlines
      ;; are handled by org?

      ;; Fix-me: Display just file names first? How is that setup?
      ;; Does org use jit-lock for this or should I fix that? Can
      ;; jit-lock handle things like this? Did Stefan suggest
      ;; something like it? Could it be handled by just request
      ;; refontification of "*" or does that trigger refontification
      ;; of the whole tail of the buffer? Is there any big
      ;; disadvantage with whole buffer refontification? Could it be
      ;; handled by post-command-hook instead? Is that even better
      ;; since this is not a head -> tail op?
      (orgstruct-mode)
      )))

(defconst idxsearch-error-regexp-alist
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
    ("^\\* File \\(.+\\) matches$" 1 nil nil 0 1)
    ;;("^File \\(.+\\) matches$" 1 nil nil 0 1)
    )
  "Regexp used to match search hits.  See `compilation-error-regexp-alist'.")

(defvar idxsearch-mode-font-lock-keywords
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
     (idxsearch-hit-marker)
     )
   "Additional things to highlight in idxsearch mode.
This gets tacked on the end of the generated expressions.")

(defvar idxsearch-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] 'idxsearch-org-open-at-point)
    map))

(defun idxsearch-hit-marker (bound)
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
          (put-text-property b2 e2 'keymap idxsearch-link-keymap)
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

(defun idxsearch-org-open-at-point ()
  (interactive)
  (let* ((file (idxsearch-find-filename))
         (full (expand-file-name file))
         (default-directory (file-name-directory full)))
    (org-open-at-point)))

(defvar idxsearch-hit-face	compilation-info-face
  "Face name to use for search hits.")

(defun idxsearch-find-filename ()
  (let ((here (point))
        (file-loc-patt "^\\* File .* matches$"))
    (unless (re-search-backward file-loc-patt nil t)
      (error "Expected to find line matching %S above" file-loc-patt))
    (forward-char 12)
    (let ((file-msg (get-text-property (point) 'message)))
      (goto-char here)
      (caar (nth 2 (nth 0 file-msg))))))

(defun idxsearch-next-error-function (n &optional reset)
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
        (setq file (idxsearch-find-filename))
        (setcar (car (nth 2 (nth 0 msg))) file)
        (goto-char msg-pt)
        (let ((inhibit-read-only t))
          (put-text-property (point) (1+ (point)) 'message msg))
        (compilation-next-error-function n reset)
        ;;(goto-line line)
        ))))

(define-compilation-mode idxsearch-mode "Search"
  "Mode for `idxsearch' output."
  (setq next-error-function 'idxsearch-next-error-function)
  (set (make-local-variable 'compilation-error-face) idxsearch-hit-face)
  ;;(set (make-local-variable 'compilation-error-regexp-alist) idxsearch-regexp-alist)
  ;;(set (make-local-variable 'compilation-process-setup-function) 'grep-process-setup)
  ;; (message "flkw=%S" compilation-mode-font-lock-keywords)
  )

(defun idxsearch-add-powershell-kw ()
  (let ((kw `((,(cadr powershell-compilation-error-regexp-alist)
              (1 'compilation-error)
              (2 compilation-line-face nil t)
              (0
               (compilation-error-properties '1 2 nil nil nil '2 'nil)
               append))))
        )
    (font-lock-add-keywords 'idxsearch-mode kw)))
(add-hook 'idxsearch-mode-hook 'idxsearch-add-powershell-kw)

(provide 'idxsearch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idxsearch.el ends here
