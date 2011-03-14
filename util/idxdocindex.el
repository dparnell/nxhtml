;;; idxdocindex.el --- Support docindexer in idxsearch.el
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2011-01-03 Mon
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
;; http://www.methods.co.nz/docindexer/
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

;; Fix-me: add line searching.

(defgroup idxdocindex nil
  "Customization group of idxdocindex."
  :group 'idxsearch)

(defcustom idxdocindex-dirs nil
  "Index roots."
  :type '(repeat directory)
  :group 'idxdocindex)

;;(setq idxdocindex-install-dir "C:/Program Files/docindexer/")
(defcustom idxdocindex-install-dir nil
  "Installation directory for docindex."
  :type 'directory
  :group 'idxdocindex)

(defcustom idxdocindex-include-files "*.pdf|*.org"
  "Value for docindex -i switch when indexing."
  :type 'string
  :group 'idxdocindex)

(defun idxdocindex-index-files ()
  ;; docindex.exe" -r root -i "*.pdf|*.org|*.html|*.htm"
  )

;;;###autoload
(defun idxdocindex-search (search-patt file-patts root)
  ;; Fix-me copy parameter handling from idxgds-search
  (let* ((index-root (catch 'root
                      (dolist (dir idxdocindex-dirs)
                        (let ((rel (file-relative-name root dir)))
                          (unless (or (string= ".." (substring rel 0 2))
                                      (file-name-absolute-p rel))
                            (throw 'root dir))))))
         (buffer-name "*idxsearch docindex*")
         (buffer (get-buffer buffer-name))
         (cnt-hits 0)
         win maxw
         exts not-exts)
    (dolist (fp file-patts)
      (if (and (string-match (rx "*." (submatch (+ (not (any ".*")))) eos)
                             fp)
               (> 6 (length (match-string 1 fp)))
               nil ;; fix-me: bug in docsearch, can only handle one ext:?
               )
          ;; We have probably got an extension
          (setq exts (cons (match-string-no-properties 1 fp) exts))
        ;; This was something else, just convert it to a regexp:
        (let* ((fp1 (replace-regexp-in-string "\\." "\\." fp t t))
               (fp2 (replace-regexp-in-string "*" ".*" fp1 t t)))
          (setq not-exts (cons fp2 not-exts)))))
    (when exts
      (setq exts (mapconcat 'identity exts " ")))
    (when not-exts
      (setq not-exts (mapconcat 'identity not-exts "\\|")))
    (when buffer (kill-buffer buffer))
    (setq buffer (get-buffer-create buffer-name))
    (setq win (display-buffer buffer))
    (setq maxw (window-width win))
    (with-current-buffer buffer
      (idxsearch-mode)
      (setq default-directory root)
      (visual-line-mode 1)
      (setq wrap-prefix "           ")
      (orgstruct-mode)
      (let ((inhibit-read-only t)
            (docsearch "docsearch"))
        (when idxdocindex-install-dir
          (setq docsearch (expand-file-name "docsearch" idxdocindex-install-dir))
          (setq docsearch (shell-quote-argument docsearch)))
        (insert "-*- mode: idxsearch; default-directory: \"" root "\" -*-\n")
        (insert "\n" docsearch " -a " search-patt " .\n")
        (insert "Search started at " (format-time-string "%Y-%m-%d %T\n\n"))

        (cond
         (  (and (not idxdocindex-install-dir)
                 (not (executable-find docsearch)))
            (insert (propertize "Error:" 'font-lock-face 'font-lock-warning-face)
                    "\nPlease ")
            (insert-text-button "customize"
                                'action
                                (lambda (btn)
                                  (customize-option 'idxdocindex-install-dir)))
            (insert " idxdocindex-install-dir to\ntell where docindexer is installed."))

         (  (not index-root)
            (insert (propertize "Error:" 'font-lock-face 'font-lock-warning-face)
                    "\nThe directory "
                    (propertize root 'font-lock-face 'font-lock-string-face)
                    " is not known to be indexed."
                    "\nIf it is then please "
                    )
            (insert-text-button "customize"
                                'action
                                (lambda (btn)
                                  (customize-option 'idxdocindex-dirs)))
            (insert " idxdocindex-dirs."))

         (t
          ;; docsearch "ext:(org pdf)" "something wrong" AND good
          (let* ((cmd (concat docsearch
                              (when exts (concat " \"ext:(" exts ")\""))
                              " -a "
                              " "
                              search-patt
                              " ."
                              ))
                 (buf (get-buffer-create "docsearch-out"))
                 (num-hits 0)
                 (debug nil))
            ;;(when debug (message "cmd=%s" cmd))
            (message "cmd=%s" cmd)
            (with-current-buffer buf
              (erase-buffer)
              (when debug (display-buffer buf))
              (call-process-shell-command cmd nil t t)
              (goto-char (point-min))
              (when (looking-at "WARNING: could not properly read security provider files:\n")
                (forward-line 4)
                (delete-region (point-min) (point)))
              (goto-char (point-min))
              (while (not (eobp))
                (let ((file (buffer-substring (point-at-bol) (point-at-eol))))
                  (when (or (not not-exts)
                            (string-match not-exts file))
                    (with-current-buffer buffer
                      (setq num-hits (1+ num-hits))
                      (setq file (file-relative-name file))
                      (insert "* File " file " matches\n")
                      (when (idxsearch-text-p file)
                        (idxsearch-grep file search-patt nil maxw))
                      (sit-for 0)
                      )))
                (forward-line)))
            (unless debug (kill-buffer buf))
            (insert (format "\nMatched %d files\n" num-hits))
            (insert "Search finished at " (format-time-string "%Y-%m-%d %T\n\n"))
            )))))))

(provide 'idxdocindex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idxdocindex.el ends here
