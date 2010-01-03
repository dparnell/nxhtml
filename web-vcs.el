;;; web-vcs.el --- Download file trees from VCS web pages
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-11-26 Thu
(defconst web-vcs:version "0.61") ;; Version:
;; Last-Updated: 2009-12-11 Fri
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
;; Update file trees within Emacs from VCS systems using information
;; on their web pages.
;;
;; See the command `nxhtml-download'.
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

(eval-when-compile (require 'cl))
(require 'hi-lock)
(require 'advice)
(require 'web-autoload nil t)

(defgroup web-vcs nil
  "Customization group for web-vcs."
  :group 'programming
  :group 'development)

(defcustom web-vcs-links-regexp
  `(
    (lp ;; Id
     ;; Comment:
     "http://www.launchpad.com/ uses this 2009-11-29 with Loggerhead 1.10 (generic?)"
     ;; Files URL regexp:
     ,(rx "href=\""
          (submatch (regexp ".*/download/[^\"]*"))
          "\"")
     ;; Dirs URL regexp:
     ,(rx "href=\""
          (submatch (regexp ".*%3A/[^\"]*/"))
          "\"")
     ;; File name URL part regexp:
     "\\([^\/]*\\)$"
     ;; Page revision regexp:
     ,(rx "for revision"
          (+ whitespace)
          "<span>"
          (submatch (+ digit))
          "</span>")
     ;; Release revision regexp:
     ,(rx "/"
          (submatch (+ digit))
          "\"" (+ (not (any ">"))) ">"
          (optional "Release ")
          (+ digit) "." (+ digit) "<")
     )
    )
  "Regexp patterns for matching links on a VCS web page.
The patterns are grouped by VCS web system type.

*Note: It is always sub match 1 from these patterns that are
       used."
  :type '(repeat
          (list
           (symbol :tag "VCS web system type specifier")
           (string :tag "Description")
           (regexp :tag "Files URL regexp")
           (regexp :tag "Dirs URL regexp")
           (regexp :tag "File name URL part regexp")
           (regexp :tag "Page revision regexp")
           (regexp :tag "Release revision regexp")
           ))
  :group 'web-vcs)

(defface web-vcs-gold
  '((((background dark)) (:background "gold" :foreground "black"))
    (t (:foreground "black" :background "gold")))
  "Face for hi-lock mode."
  :group 'web-vcs)

(defface web-vcs-red
  '((((background dark)) (:background "red" :foreground "black"))
    (t (:foreground "black" :background "#f86")))
  "Face for hi-lock mode."
  :group 'web-vcs)

(defface web-vcs-green
  '((((background dark)) (:background "red" :foreground "black"))
    (t (:foreground "black" :background "#8f6")))
  "Face for hi-lock mode."
  :group 'web-vcs)

(defcustom web-vcs-default-download-directory
  '~/.emacs.d/
  "Default download directory."
  :type '(choice (const :tag "~/.emacs.d/" '~/.emacs.d/)
                 (const :tag "Fist site-lisp in `load-path'" 'site-lisp-dir)
                 (const :tag "Directory where `site-run-file' lives" 'site-run-dir)
                 (string :tag "Specify directory"))
  :group 'web-vcs)

;;(web-vcs-default-download-directory)
(defun web-vcs-default-download-directory ()
  "Try to find a suitable place.
Considers site-start.el, site-
"
  (let ((site-run-dir (file-name-directory (locate-library site-run-file)))
        (site-lisp-dir (catch 'first-site-lisp
                         (dolist (d load-path)
                           (let ((dir (file-name-nondirectory (directory-file-name d))))
                             (when (string= dir "site-lisp")
                               (throw 'first-site-lisp (file-name-as-directory d)))))))
        )
    (message "site-run-dir=%S site-lisp-dir=%S" site-run-dir site-lisp-dir)
    (case web-vcs-default-download-directory
      ('~/.emacs.d/ "~/.emacs.d/")
      ('site-lisp-dir site-lisp-dir)
      ('site-run-dir site-run-dir)
      (t web-vcs-default-download-directory))
    ))


;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/" t "c:/test/temp13/" t)
;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/util/" t "temp" t)
;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/alts/" t "temp" t)


;;;###autoload
(defun web-vcs-get-files-from-root (web-vcs url dl-dir)
  "Download a file tree from VCS system using the web interface.
Use WEB-VCS entry in variable `web-vcs-links-regexp' to download
files via http from URL to directory DL-DIR.

Show URL first and offer to visit the page.  That page will give
you information about version control system \(VCS) system used
etc."
  (unless (web-vcs-contains-moved-files dl-dir)
    (when (if (not (y-or-n-p (concat "Download files from \"" url "\".\n"
                                     "You can see on that page which files will be downloaded.\n\n"
                                     "Visit that page before downloading? ")))
              t
            (browse-url url)
            (if (y-or-n-p "Start downloading? ")
                t
              (message "Aborted")
              nil))
      (message "")
      (web-vcs-get-files-on-page web-vcs url t (file-name-as-directory dl-dir) nil))))

(defun web-vcs-get-missing-matching-files (web-vcs url dl-dir files-regexp)
  "Download missing files from VCS system using the web interface.
Use WEB-VCS entry in variable `web-vcs-links-regexp' to download
files via http from URL to directory DL-DIR.

Before downloading offer to visit the page from which the
downloading will be made.
"
  (let ((vcs-rec (or (assq web-vcs web-vcs-links-regexp)
                     (error "Does not know web-cvs %S" web-vcs))))
    (web-vcs-get-files-on-page-1 vcs-rec url dl-dir "" files-regexp 0 nil nil)))

(defun web-vcs-get-files-on-page (web-vcs url recursive dl-dir test)
  "Download files listed by WEB-VCS on web page URL.
WEB-VCS is a specifier in `web-vcs-links-regexp'.

If RECURSIVE go into sub folders on the web page and download
files from them too.

Place the files under DL-DIR.

Before downloading check if the downloaded revision already is
the same as the one on the web page.  This is stored in the file
web-vcs-revision.txt.  After downloading update this file.

If TEST is non-nil then do not download, just list the files."
  (require 'hi-lock) ;; For faces
  (unless (string= dl-dir (file-name-as-directory (expand-file-name dl-dir)))
    (error "Download dir dl-dir=%S must be a full directory path" dl-dir))
  (catch 'command-level
    (when (web-vcs-contains-moved-files dl-dir)
      (throw 'command-level nil))
    (let ((vcs-rec (or (assq web-vcs web-vcs-links-regexp)
                       (error "Does not know web-cvs %S" web-vcs)))
          (start-time (current-time)))
      (unless (file-directory-p dl-dir)
        (if (yes-or-no-p (format "Directory %S does not exist, create it? "
                                 (file-name-as-directory
                                  (expand-file-name dl-dir))))
            (mkdir dl-dir t)
          (message "Can't download then")
          (throw 'command-level nil)))
      (let ((old-win (selected-window)))
        (unless (eq (get-buffer "*Messages*") (window-buffer old-win))
          (switch-to-buffer-other-window "*Messages*"))
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize (format "\n\nWeb-Vcs Download: %S\n" url) 'face 'web-vcs-gold))
        (insert "\n")
        (redisplay t)
        (set-window-point (selected-window) (point-max))
        (select-window old-win))
      (let* ((rev-file (expand-file-name "web-vcs-revision.txt" dl-dir))
             (rev-buf (find-file-noselect rev-file))
             ;; Fix-me: Per web vcs speficier.
             (old-rev-range (with-current-buffer rev-buf
                              (widen)
                              (goto-char (point-min))
                              (when (re-search-forward (format "%s:\\(.*\\)\n" web-vcs) nil t)
                                ;;(buffer-substring-no-properties (point-min) (line-end-position))
                                ;;(match-string 1)
                                (cons (match-beginning 1) (match-end 1))
                                )))
             (old-revision (when old-rev-range
                             (with-current-buffer rev-buf
                               (buffer-substring-no-properties (car old-rev-range)
                                                               (cdr old-rev-range)))))
             (dl-revision (web-vcs-get-revision-on-page vcs-rec url))
             ret
             moved)
        (when (and old-revision (string= old-revision dl-revision))
          (when (y-or-n-p (format "You already have revision %s.  Quit? " dl-revision))
            (message "Aborted")
            (kill-buffer rev-buf)
            (throw 'command-level nil)))
        ;; We do not have a revision number once we start download.
        (with-current-buffer rev-buf
          (when old-rev-range
            (delete-region (car old-rev-range) (cdr old-rev-range))
            (basic-save-buffer)))
        (setq ret (web-vcs-get-files-on-page-1
                   vcs-rec url
                   dl-dir
                   ""
                   nil
                   (if recursive 0 nil)
                   dl-revision test))
        (setq moved       (nth 1 ret))
        ;; Now we have a revision number again.
        (with-current-buffer rev-buf
          (when (= 0 (buffer-size))
            (insert "WEB VCS Revisions\n\n"))
          (goto-char (point-max))
          (unless (eolp) (insert "\n"))
          (insert (format "%s:%s\n" web-vcs dl-revision))
          (basic-save-buffer)
          (kill-buffer))
        (message "-----------------")
        (web-vcs-message-with-face 'web-vcs-gold "Web-Vcs Download Ready: %S" url)
        (web-vcs-message-with-face 'web-vcs-gold "  Time elapsed: %S"
                                   (web-vcs-nice-elapsed start-time (current-time)))
        (when (> moved 0)
          (web-vcs-message-with-face 'hi-yellow
                                     "  %i files updated (old versions renamed to *.moved)"
                                     moved))))))


(defun web-vcs-get-files-on-page-1 (vcs-rec url dl-root dl-relative file-mask recursive dl-revision test)
  "Download files listed by VCS-REC on web page URL.
VCS-REC should be an entry like the entries in the list
`web-vcs-links-regexp'.

If FILE-MASK is non nil then it should be a file path.  Only
files matching this path will be downloaded then.  Each part of
the path may be a regular expresion \(not containing /).

If RECURSIVE go into sub folders on the web page and download
files from them too.

Place the files under DL-DIR.

The revision on the page URL should match DL-REVISION if this is non-nil.

If TEST is non-nil then do not download, just list the files"
  ;;(message "web-vcs-get-files-on-page-1 %s %s %s %s %s %s %s %s" vcs-rec url dl-root dl-relative file-mask recursive dl-revision test)
  (web-vcs-message-with-face 'font-lock-comment-face "web-vcs-get-files-on-page-1 %S %S %S %S" url dl-root dl-relative file-mask)
  (let* ((files-href-regexp  (nth 2 vcs-rec))
         (dirs-href-regexp   (nth 3 vcs-rec))
         (file-name-regexp   (nth 4 vcs-rec))
         (revision-regexp    (nth 5 vcs-rec))
         (dl-dir (file-name-as-directory (expand-file-name dl-relative dl-root)))
         (lst-dl-relative (web-vcs-file-name-as-list dl-relative))
         (lst-file-mask   (web-vcs-file-name-as-list file-mask))
         (url-buf (url-retrieve-synchronously url))
         this-page-revision
         files
         suburls
         (moved 0)
         (temp-file (expand-file-name "web-vcs-temp.tmp" dl-dir)))
    (with-current-buffer url-buf
      (goto-char (point-min))
      (unless (looking-at "HTTP/.* 200 OK\n")
        (let ((status "Status unknown"))
          (when (looking-at "HTTP/.* \\(.*\\)\n")
            (setq status (match-string 1)))
          (switch-to-buffer url-buf)
          (web-vcs-message-with-face 'web-vcs-red "Download error (%s): %S" status url))
        (throw 'command-level nil))
      (unless (file-directory-p dl-dir)
        (make-directory dl-dir t))
      ;; Get revision number
      (when dl-revision
        (setq this-page-revision (web-vcs-get-revision-from-url-buf vcs-rec url-buf url)))
      (when dl-revision
        (unless (string= dl-revision this-page-revision)
          (web-vcs-message-with-face 'web-vcs-red "Revision on %S is %S, but should be %S"
                                     url this-page-revision dl-revision)
          (throw 'command-level nil)))
      ;; Find files
      (goto-char (point-min))
      (while (re-search-forward files-href-regexp nil t)
        (add-to-list 'files (match-string 1)))
      ;; Find subdirs
      (when recursive
        (goto-char (point-min))
        (while (re-search-forward dirs-href-regexp nil t)
          (let ((suburl (match-string 1))
                (lenurl (length url)))
            (when (and (> (length suburl) lenurl)
                       (string= (substring suburl 0 lenurl) url))
              (add-to-list 'suburls suburl)))))
      (kill-buffer))
    ;; Download files
    (dolist (file (reverse files))
      (let* ((file-url file)
             (file-name (progn
                          (when (string-match file-name-regexp file-url)
                            (match-string 1 file-url))))
             (lst-file-name (web-vcs-file-name-as-list file-name))
             (file-dl-name (expand-file-name file-name dl-dir))
             (file-rel-name (file-relative-name file-dl-name dl-root))
             temp-buf)
        (when (or (not file-mask)
                  (web-vcs-match-folderwise file-mask file-rel-name))
          (if test
              (progn
                (message "TEST file-url=%S" file-url)
                (message "TEST file-name=%S" file-name)
                (message "TEST file-dl-name=%S" file-dl-name)
                )
            (while (setq temp-buf (find-buffer-visiting temp-file))
              (set-buffer-modified-p nil)
              (kill-buffer temp-buf))
            ;;(web-vcs-message-with-face 'font-lock-comment-face "Starting url-copy-file %S %S t t" file-url temp-file)
            (when (file-exists-p temp-file) (delete-file temp-file))
            (url-copy-file file-url temp-file t t) ;; overwrite, keep time
            (unless (or (file-exists-p temp-file)
                        (= 0 (nth 7 (file-attributes temp-file)))) ;; file size 0
              (web-vcs-message-with-face 'web-vcs-red "Failed url-copy-file %S %S t t" file-url temp-file)
              (throw 'command-level nil))
            ;;(web-vcs-message-with-face 'font-lock-comment-face "Finished url-copy-file %S %S t t" file-url temp-file)
            (let* ((time-after-url-copy (current-time))
                   (old-exists (file-exists-p file-dl-name))
                   (old-buf-open (find-buffer-visiting file-dl-name)))
              (when old-buf-open
                (when (buffer-modified-p old-buf-open)
                  (save-excursion
                    (switch-to-buffer old-buf-open)
                    (when (y-or-n-p (format "Buffer %S is modified, save to make a backup? "
                                            file-dl-name))
                      (save-buffer)))))
              (if (and old-exists
                       (web-vcs-equal-files file-dl-name temp-file))
                  (web-vcs-message-with-face 'web-vcs-green "File %S was ok" file-dl-name)
                (when old-exists
                  (let ((backup (concat file-dl-name ".moved")))
                    (when (file-exists-p backup)
                      (delete-file backup))
                    (rename-file file-dl-name backup)))
                ;;(web-vcs-message-with-face 'font-lock-comment-face "Doing rename-file %S %S" temp-file file-dl-name)
                (rename-file temp-file file-dl-name)
                (if old-exists
                    (web-vcs-message-with-face 'hi-yellow "Updated %S" file-dl-name)
                  (web-vcs-message-with-face 'web-vcs-green "Downloaded %S" file-dl-name))
                (when old-buf-open
                  (with-current-buffer old-buf-open
                    (set-buffer-modified-p nil)
                    (revert-buffer))))
              (let* ((msg-win (get-buffer-window "*Messages*")))
                (with-current-buffer "*Messages*"
                  (set-window-point msg-win (point-max))))
              (redisplay t)
              ;; This is both for user and remote server load.  Do not remove this.
              (sit-for (- 1.0 (float-time (time-subtract (current-time) time-after-url-copy))))
              ;; (unless old-buf-open
              ;;   (when old-buf
              ;;     (kill-buffer old-buf)))
              )))
        (redisplay t)))
    ;; Download subdirs
    (when suburls
      (dolist (suburl (reverse suburls))
        (let* ((dl-sub-dir (substring suburl (length url)))
               (full-dl-sub-dir (file-name-as-directory
                                 (expand-file-name dl-sub-dir dl-dir)))
               (rel-dl-sub-dir (file-relative-name full-dl-sub-dir dl-root)))
          (when (or (not file-mask)
                    (web-vcs-match-folderwise file-mask rel-dl-sub-dir))
            (unless (web-vcs-contains-file dl-dir full-dl-sub-dir)
              (error "Subdir %S not in %S" dl-sub-dir dl-dir))
            (let* ((ret (web-vcs-get-files-on-page-1 vcs-rec
                                                     suburl
                                                     dl-root
                                                     rel-dl-sub-dir
                                                     file-mask
                                                     (1+ recursive)
                                                     this-page-revision
                                                     test)))
              (setq moved (+ moved (nth 1 ret)))
              )))))
    (list this-page-revision moved)
    ))


(defun web-vcs-get-revision-on-page (vcs-rec url)
  "Get revision number using VCS-REC on page URL.
VCS-REC should be an entry like the entries in the list
`web-vcs-links-regexp'."
  ;; url-insert-file-contents
  (let ((url-buf (url-retrieve-synchronously url)))
    (web-vcs-get-revision-from-url-buf vcs-rec url-buf url)))

(defun web-vcs-get-revision-from-url-buf (vcs-rec url-buf url)
  "Get revision number using VCS-REC.
VCS-REC should be an entry in the list `web-vcs-links-regexp'.
The buffer URL-BUF should contain the content on page URL."
  (let ((revision-regexp    (nth 5 vcs-rec)))
    ;; Get revision number
    (with-current-buffer url-buf
      (goto-char (point-min))
      (if (not (re-search-forward revision-regexp nil t))
          (progn
            (web-vcs-message-with-face 'web-vcs-red "Can't find revision number on %S" url)
            (throw 'command-level nil))
        (match-string 1)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

;;(web-vcs-file-name-as-list "/a/b/c.el")
;;(web-vcs-file-name-as-list "a/b/c.el")
;;(web-vcs-file-name-as-list "c:/a/b/c.el")
;;(web-vcs-file-name-as-list ".*/a/c/")
;;(web-vcs-file-name-as-list "[^/]*/a/c/") ;; Just avoid this.
(defun web-vcs-file-name-as-list (filename)
  "Split file name FILENAME into a list with file names."
  (let ((lst-name nil)
        (head filename)
        (old-head ""))
    (while (and (not (string= old-head head))
                (> (length head) 0))
      (let* ((file-head (directory-file-name head))
             (tail (file-name-nondirectory (directory-file-name head))))
        (setq old-head head)
        (setq head (file-name-directory file-head))
        ;; For an abs path the final tail is "", use root instead:
        (when (= 0 (length tail))
          (setq tail head))
        (setq lst-name (cons tail lst-name))))
    lst-name))

;;(web-vcs-match-folderwise ".*/util/mum.el" "top/util/mum.el")
;;(web-vcs-match-folderwise ".*/util/mu.el" "top/util/mum.el")
;;(web-vcs-match-folderwise ".*/ut/mum.el" "top/util/mum.el")
;;(web-vcs-match-folderwise ".*/ut../mum.el" "top/util/mum.el")
;;(web-vcs-match-folderwise ".*/ut../mum.el" "top/util")
;;(web-vcs-match-folderwise ".*/ut../mum.el" "top")
;;(web-vcs-match-folderwise "top/ut../mum.el" "top")
;;(web-vcs-match-folderwise "util/web-autoload-2.el" "util/nxhtml-company-mode/")
(defun web-vcs-match-folderwise (regex file)
  "Split REGEXP as a file path and match against FILE parts."
  ;;(message "folderwise %S %S" regex file)
  (let ((lst-regex (web-vcs-file-name-as-list regex))
        (lst-file  (web-vcs-file-name-as-list file)))
    (when (>= (length lst-regex) (length lst-file))
      (catch 'match
        (while lst-file
          (let ((head-file  (car lst-file))
                (head-regex (car lst-regex)))
            (unless (string-match-p (concat "^" head-regex "$") head-file)
              (throw 'match nil)))
          (setq lst-file  (cdr lst-file))
          (setq lst-regex (cdr lst-regex)))
        t))))

(defun web-vcs-contains-file (dir file)
  "Return t if DIR contain FILE."
  (assert (string= dir (file-name-as-directory (expand-file-name dir))) t)
  (assert (or (string= file (file-name-as-directory (expand-file-name file)))
              (string= file (expand-file-name file))) t)
  (let ((dir-len (length dir)))
    (assert (string= "/" (substring dir (1- dir-len))))
    (when (> (length file) dir-len)
      (string= dir (substring file 0 dir-len)))))

(defun web-vcs-nice-elapsed (start-time end-time)
  "Format elapsed time between START-TIME and END-TIME nicely.
Those times should have the same format as time returned by
`current-time'."
  (format-seconds "%h h %m m %z%s s" (float-time (time-subtract end-time start-time))))

;; (web-vcs-equal-files "web-vcs.el" "temp.tmp")
;; (web-vcs-equal-files "../.nosearch" "temp.tmp")
(defun web-vcs-equal-files (file-a file-b)
  "Return t if files FILE-A and FILE-B are equal."
  (let* ((cmd (if (eq system-type 'windows-nt)
                  (list "fc" nil nil nil
                        "/B" "/OFF"
                        (convert-standard-filename file-a)
                        (convert-standard-filename file-b))
                (list diff-command nil nil nil
                      "--binary" "-q" file-a file-b)))
         (ret (apply 'call-process cmd)))
    ;;(message "ret=%s, cmd=%S" ret cmd) (sit-for 2)
    (cond
     ((= 1 ret)
      nil)
     ((= 0 ret)
      t)
     (t
      (error "%S returned %d" cmd ret)))))

;; (web-vcs-message-with-face 'secondary-selection "I am saying: %s and %s" "Hi" "Farwell!")
(defun web-vcs-message-with-face (face format-string &rest args)
  "Display a colored message at the bottom of the string.
FACE is the face to use for the message.
FORMAT-STRING and ARGS are the same as for `message'.

Also put FACE on the message in *Messages* buffer."
  (with-current-buffer "*Messages*"
    (save-restriction
      (widen)
      (let* ((start (let ((here (point)))
                      (goto-char (point-max))
                      (prog1
                          (copy-marker
                           (if (bolp) (point-max)
                             (1+ (point-max))))
                        (goto-char here))))
             (msg-with-face (propertize (apply 'format format-string args)
                                        'face face)))
        ;; This is for the echo area:
        (message "%s" msg-with-face)
        ;; This is for the buffer:
        (when (< 0 (length msg-with-face))
          (goto-char (1- (point-max)))
          ;;(backward-char)
          ;;(unless (eolp) (goto-char (line-end-position)))
          (put-text-property start (point)
                             'face face))))))

;; (web-vcs-num-moved "c:/emacs/p/091105/EmacsW32/nxhtml/")
(defun web-vcs-num-moved (root)
  "Return nof files matching *.moved inside directory ROOT."
  (let* ((file-regexp ".*\\.moved$")
         (files (directory-files root t file-regexp))
         (subdirs (directory-files root t)))
    (dolist (subdir subdirs)
      (when (and (file-directory-p subdir)
                 (not (or (string= "/." (substring subdir -2))
                          (string= "/.." (substring subdir -3)))))
        (setq files (append files (web-vcs-rdir-get-files subdir file-regexp) nil))))
    (length files)))

;; Copy of rdir-get-files in ourcomment-util.el
(defun web-vcs-rdir-get-files (root file-regexp)
  (let ((files (directory-files root t file-regexp))
        (subdirs (directory-files root t)))
    (dolist (subdir subdirs)
      (when (and (file-directory-p subdir)
                 (not (or (string= "/." (substring subdir -2))
                          (string= "/.." (substring subdir -3)))))
        (setq files (append files (web-vcs-rdir-get-files subdir file-regexp) nil))))
    files))

(defun web-vcs-contains-moved-files (dl-dir)
  "Return t if there are *.moved files in DL-DIR."
  (let ((num-moved (web-vcs-num-moved dl-dir)))
    (when (> num-moved 0)
      (web-vcs-message-with-face 'font-lock-warning-face
                                 (concat "There are %d *.moved files (probably from prev download)\n"
                                         "in %S.\nPlease delete them first.")
                                 num-moved dl-dir)
      t)))





(defun web-vcs-set&save-option (symbol value)
  (customize-set-variable symbol value)
  (customize-set-value symbol value)
  (when (condition-case nil (custom-file) (error nil))
    (customize-mark-to-save symbol)
    (custom-save-all)
    (message "web-vcs: Saved option %s with value %s" symbol value)))

(defvar web-vcs-el-this (or load-file-name
                            (when (boundp 'bytecomp-filename) bytecomp-filename)
                            buffer-file-name))


(require 'bytecomp)
(defun web-vcs-byte-compile-newer-file (el-file load)
  (let ((elc-file (byte-compile-dest-file el-file)))
    (when (or (not (file-exists-p elc-file))
              (file-newer-than-file-p el-file elc-file))
      (byte-compile-file el-file load))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific for nXhtml

(defvar web-vcs-nxhtml-base-url "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/")

;; Fix-me: make gen for 'lp etc
(defun nxhtml-download-root-url (revision)
  (let* ((base-url web-vcs-nxhtml-base-url)
         (files-url (concat base-url "files/"))
         (rev-part (if revision (number-to-string revision) "head%3A/")))
    (concat files-url rev-part)))


;;(nxhtml-default-download-directory)
(defun nxhtml-default-download-directory ()
  (let* ((ur (expand-file-name "" "~"))
         (ur-len (length ur))
         (full (if (and (boundp 'nxhtml-install-dir)
                        nxhtml-install-dir)
                   nxhtml-install-dir
                 (file-name-as-directory
                  (expand-file-name "nxhtml"
                                    (web-vcs-default-download-directory)))))
         (full-len (length full)))
    (if (and (> full-len ur-len)
             (string= ur (substring full 0 ur-len)))
        (concat "~" (substring full ur-len))
      full)))

(eval-when-compile (require 'cl))
;;(call-interactively 'nxhtml-setup-install)
;; (read-key "Prompt: ")
;; (y-or-n-p "Prompt")
;;;###autoload
(defun nxhtml-setup-install (way)
  "Setup and start nXhtml installation.

There are two different ways to do it:

  (1) Download all at once: `nxhtml-setup-download-all'
  (2) Automatically download part by part: `nxhtml-setup-auto-download'

You can convert between those ways by calling this function again.

To learn more about nXhtml visit its home page at URL
`http://www.emacswiki.com/NxhtmlMode/'."
  (interactive (let ((curr-cfg (current-window-configuration)))
                 (list
                  (let* ((key nil)
                         (current-way (if (and (boundp 'nxhtml-install-dir)
                                               nxhtml-install-dir)
                                          (if (and (boundp 'nxhtml-autoload-web)
                                                   nxhtml-autoload-web)
                                              "Your current setup is to download part by part from the web."
                                            "Your current setup it to download all of nXhtml at once.")
                                        "(You have not currently installed nXhtml.)"))
                         (prompt (concat "Setup nXhtml install."
                                         "\n" current-way
                                         "\n"
                                         "\n(1) Download whole at once, or (2) part by part as needed?"
                                         "\n(? for help, q to quit): "))
                         (please nil))
                    (while (not (member key '(?1 ?2 ?q 7)))
                      (if (not (member key '(??)))
                          (when key
                            (unless please
                              (setq prompt (concat "Please answer with one of the alternatives.\n\n"
                                                   prompt))
                              (setq please t)))
                        (describe-function 'nxhtml-setup-install)
                        (select-window (get-buffer-window (help-buffer)))
                        (delete-other-windows))
                      (setq key (my-read-key prompt))
                      ;;(message "key = %S" key) (sit-for 1)
                      )
                    (case key
                      (7 (set-window-configuration curr-cfg)
                          nil)
                      (?1 'whole)
                      (?2 'part-by-part))))))
  (message "")
  (case way
    (part-by-part (call-interactively 'nxhtml-setup-auto-download))
    (whole (call-interactively 'nxhtml-setup-download-all))
    ((eq nil way) nil)
    (t (error "Unknown way = %S" way))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temporary helpers, possibly included in Emacs

(defun my-read-and-accept-key (prompt accepted &optional reject-message help-function)
  (let ((key nil)
        rejected)
    (while (not (member key accepted))
      (if (and help-function
               (or (member key help-event-list)
                   (eq key ??)))
          (funcall help-function)
        (unless rejected
          (setq rejected t)
          (setq prompt (concat (or reject-message "Please answer with one of the alternatives.")
                               "\n\n"
                               prompt))
          (setq key (my-read-key prompt)))))
    key))

(defun my-read-key (&optional prompt)
  "Read a key from the keyboard.
Contrary to `read-event' this will not return a raw event but instead will
obey the input decoding and translations usually done by `read-key-sequence'.
So escape sequences and keyboard encoding are taken into account.
When there's an ambiguity because the key looks like the prefix of
some sort of escape sequence, the ambiguity is resolved via `read-key-delay'."
  (let ((overriding-terminal-local-map read-key-empty-map)
	(overriding-local-map nil)
	(old-global-map (current-global-map))
        (timer (run-with-idle-timer
                ;; Wait long enough that Emacs has the time to receive and
                ;; process all the raw events associated with the single-key.
                ;; But don't wait too long, or the user may find the delay
                ;; annoying (or keep hitting more keys which may then get
                ;; lost or misinterpreted).
                ;; This is only relevant for keys which Emacs perceives as
                ;; "prefixes", such as C-x (because of the C-x 8 map in
                ;; key-translate-table and the C-x @ map in function-key-map)
                ;; or ESC (because of terminal escape sequences in
                ;; input-decode-map).
                read-key-delay t
                (lambda ()
                  (let ((keys (this-command-keys-vector)))
                    (unless (zerop (length keys))
                      ;; `keys' is non-empty, so the user has hit at least
                      ;; one key; there's no point waiting any longer, even
                      ;; though read-key-sequence thinks we should wait
                      ;; for more input to decide how to interpret the
                      ;; current input.
                      (throw 'read-key keys)))))))
    (unwind-protect
        (progn
	  (use-global-map read-key-empty-map)
          (message (concat (apply 'propertize prompt (member 'face minibuffer-prompt-properties))
                           (propertize " " 'face 'cursor)))
	  (aref	(catch 'read-key (read-key-sequence-vector nil nil t)) 0))
      (cancel-timer timer)
      (use-global-map old-global-map))))

;; End temp helpers
;;;;;;;;;;;;;;;;;;;;;;;;

;;(web-vcs-read-nxhtml-dl-dir "Test")
(defun web-vcs-read-nxhtml-dl-dir (prompt)
  "Return current nXhtml install dir or read dir."
  (or (and (boundp 'nxhtml-install-dir)
           nxhtml-install-dir)
      (let* ((pr (concat
                  "A directory named 'nxhtml' will be created below the root you give."
                  "\n"
                  prompt))
             (root (read-directory-name pr (nxhtml-default-download-directory))))
        (when root
          (expand-file-name "nxhtml" root)))))

(defvar nxhtml-handheld-wincfg nil)
(defun nxhtml-handheld-restore-wincg ()
  (when nxhtml-handheld-wincfg
    (set-window-configuration nxhtml-handheld-wincfg)
    (setq nxhtml-handheld-wincfg nil)))

;;(nxhtml-handheld-add-loading-to-custom-file "TEST-ME")
(defun nxhtml-handheld-add-loading-to-custom-file (file-to-load)
  (setq nxhtml-handheld-wincfg (current-window-configuration))
  (delete-other-windows)
  (let ((info-buf (get-buffer-create "Information about how to add nXhtml to (custom-file)"))
        (load-str (format "(load %S)" file-to-load)))
    (with-current-buffer info-buf
      (add-hook 'kill-buffer-hook 'nxhtml-handheld-restore-wincg nil t)
      (insert "Insert the following line to (custom-file), ie the file in the other window:\n\n")
      (let ((here (point)))
        (insert "  "
                (propertize load-str 'face 'secondary-selection)
                "\n")
        (copy-region-as-kill here (point))
        (insert "\nThe line above is in the clipboard so you can just paste it where you want it.\n")
        (insert "When ready kill this buffer.")
        (goto-char here))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))
    (set-window-buffer (selected-window) info-buf)
    (find-file-other-window (custom-file))
    ))

;; (nxhtml-add-loading-to-custom-file "test-file")
(defun nxhtml-add-loading-to-custom-file (file-to-load part-by-part)
  (message "")
  (if (not (and (fboundp 'custom-file)
                (condition-case nil (custom-file) (error nil))))
      (progn
        (message "")
        (web-vcs-message-with-face 'web-vcs-red
                                   (concat
                                    "To finish the setup of nXhtml you must add"
                                    "\n\n  (load %S)"
                                    "\n\nto your custom-file if you have not done it yet."
                                    "\n\nYou must also customize the variable `nxhtml-autoload-web'.\n")
                                   file-to-load)
        (message ""))
    (let ((prompt (concat "Basic setup of nXhtml is done, but it must be loaded from (custom-file)."
                          "\nShould I add loading of nXhtml to (custom-file) for you? ")))
      (if (yes-or-no-p prompt)
          (nxhtml-add-loading-to-custom-file-auto file-to-load)
        (if (yes-or-no-p "Should I guide you through how to do it? ")
            (nxhtml-handheld-add-loading-to-custom-file file-to-load)
          (web-vcs-message-with-face 'web-vcs-green
                                     "OK. You need to add (load %S) to your init file" file-to-load))))))

;; Fix-me: really do this? Is it safe enough?
(defun nxhtml-add-loading-to-custom-file-auto (file-to-load)
  (unless (file-name-absolute-p file-to-load)
    (error "nxhtml-add-loading-to-custom-file: Not abs file name: %S" file-to-load))
  (let ((old-buf (find-buffer-visiting (custom-file)))
        (full-to-load (expand-file-name file-to-load)))
    (with-current-buffer (or old-buf (find-file-noselect (custom-file)))
      (save-restriction
        (widen)
        (catch 'done
          (while (progn
                   (while (progn (skip-chars-forward " \t\n\^l")
                                 (looking-at ";"))
                     (forward-line 1))
                   (not (eobp)))
            (let ((start (point))
                  (form (read (current-buffer))))
              (when (eq (nth 0 form) 'load)
                (let* ((form-file (nth 1 form))
                       (full-form-file (expand-file-name form-file)))
                  (when (string= full-form-file full-to-load)
                    (throw 'done nil))
                  (when (and (string= (file-name-nondirectory full-form-file)
                                      (file-name-nondirectory full-to-load))
                             (not (string= full-form-file full-to-load)))
                    (if (yes-or-no-p "Replace current nXhtml loading in (custom-file)? ")
                        (progn
                          (goto-char start) ;; at form start now
                          (forward-char (length "(load "))
                          (skip-chars-forward " \t\n\^l") ;; at start of string
                          (setq start (point))
                          (setq form (read (current-buffer)))
                          (delete-region start (point))
                          (insert (format "%S" full-to-load))
                          (basic-save-buffer))
                      (web-vcs-message-with-face 'web-vcs-red "Can't continue then")
                      (throw 'command-level nil)))))))
          ;; At end of file
          (insert (format "\n(load  %S)\n" file-to-load))
          (basic-save-buffer))
        (unless old-buf (kill-buffer old-buf))))))

(defvar nxhtml-basic-files '("web-autoload.el"
                             ;;"nxhtml-auto-helpers.el"
                             "nxhtml-loaddefs.el"
                             "autostart.el"
                             ;;"web-autostart.el"
                             "etc/schema/schema-path-patch.el"
                             "nxhtml/nxhtml-autoload.el"))

(defun nxhtml-check-convert-to-part-by-part ()
  (when (and (boundp 'nxhtml-install-dir)
             nxhtml-install-dir)
    (unless (and (boundp 'nxhtml-autoload-web)
                 nxhtml-autoload-web)
      (if (not (boundp 'nxhtml-menu:version))
          (error "nxhtml-install-dir set but no version found")
        (unless (string-match "[\.0-9]+" nxhtml-menu:version)
          (error "Can't find current version nxhtml-menu:version=%S" nxhtml-menu:version))
        (let* ((ver-str (match-string 0 nxhtml-menu:version))
               (ver-num (string-to-number ver-str)))
          (when (< ver-num 2.07)
            (web-vcs-message-with-face 'web-vcs-red "Too old nXhtml for download part by part.")
            (throw 'command-level nil)))))))

;;;###autoload
(defun nxhtml-setup-auto-download (dl-dir)
  "Set up to autoload nXhtml files from the web.

This function will download some initial files and then setup to
download the rest when you need them.

Files will be downloaded under the directory root you specify in
DL-DIR.

If you already have nXhtml installed and loaded this function
will only let you download to that directory.  In that case you
can use this function to upgrade individual files.  Just delete
them and they will be downloaded as you need them.  \(But beware
that sometimes there might be problem because the files gets out
of phase.  A future version will try to take care of this.)

You may switch by this mode of downloading or downloading the
whole of nXhtml by once.  To switch just call the command
`nxhtml-setup-install'.

See also the command `nxhtml-setup-download-all'.

Note: If your nXhtml is to old you can't use this function
      directly.  You have to upgrade first, se the function
      above."
  (interactive (progn
                 (describe-function 'nxhtml-setup-auto-download)
                 (select-window (get-buffer-window (help-buffer)))
                 (delete-other-windows)
                 (nxhtml-check-convert-to-part-by-part)
                 (list
                  (catch 'command-level
                    (unless (yes-or-no-p "Convert to updating nXhtml part by part? ")
                      (throw 'command-level nil))
                    (web-vcs-read-nxhtml-dl-dir "Download nXhtml part by part to directory")))))
  (if (not dl-dir)
      (unless (called-interactively-p)
        (error "dl-dir show be a directory"))
    (nxhtml-check-convert-to-part-by-part)
    (when (and (boundp 'nxhtml-install-dir)
               nxhtml-install-dir)
      (unless (string= (file-truename dl-dir)
                       (file-truename nxhtml-install-dir))
        (error "Download dir must be same as nxhtml-install-dir=%S" nxhtml-install-dir)))
    (let* (;; Need some files:
           (web-vcs-el-src (concat (file-name-sans-extension web-vcs-el-this) ".el"))
           (web-vcs-el (expand-file-name (file-name-nondirectory web-vcs-el-src)
                                         dl-dir))
           (vcs 'lp)
           (base-url (nxhtml-download-root-url nil))
           (byte-comp (if (boundp 'web-autoload-autocompile)
                          web-autoload-autocompile
                        t))
           (has-nxhtml (and (boundp 'nxhtml-install-dir)
                            nxhtml-install-dir)))
      (unless (file-exists-p dl-dir)
        (if (yes-or-no-p (format "Directory %S does not exist, create it? " dl-dir))
            (make-directory dl-dir t)
          (error "Aborted by user")))
      (setq message-log-max t)
      (message "")
      (web-vcs-message-with-face 'web-vcs-green "==== Starting nXhtml part by part state ====")
      (unless (file-exists-p web-vcs-el)
        (copy-file web-vcs-el-src web-vcs-el))
      (when byte-comp
        ;; Fix-me: check age
        (web-vcs-byte-compile-newer-file web-vcs-el t))
      (catch 'command-level
        (dolist (file nxhtml-basic-files)
          (let ((dl-file (expand-file-name file dl-dir)))
            (unless (file-exists-p dl-file)
              (web-vcs-get-missing-matching-files vcs base-url dl-dir file))))
        ;; Autostart.el has not run yet, add current dir to load-path.
        (let ((load-path (cons (file-name-directory web-vcs-el) load-path)))
          (when byte-comp
            (dolist (file nxhtml-basic-files)
              (let ((el-file (expand-file-name file dl-dir)))
                ;; Fix-me: check age
                (message "maybe bytecomp %S" el-file)
                (when (boundp 'majmodpri-sort-after-load)
                  (message "majmodpri-sort-after-load =%S" majmodpri-sort-after-load))
                (web-vcs-byte-compile-newer-file el-file nil)))))
        (let ((autostart-file (expand-file-name "autostart" dl-dir)))
          ;;(ad-deactivate 'require)
          (web-vcs-set&save-option 'nxhtml-autoload-web t)
          (message "before load %S" autostart-file)
          (load autostart-file)
          (unless (ad-is-active 'require) (ad-activate 'require))
          (message "after load %S" autostart-file)
          (unless has-nxhtml (nxhtml-add-loading-to-custom-file autostart-file t)))))))

;;(call-interactively 'nxhtml-download)
;;;###autoload
(defun nxhtml-setup-download-all (dl-dir)
  "Download or update all of nXhtml.

If you already have nXhtml installed you can update it with this
command. If you want to update only certain files you can do so
by switching \(maybe temporary) to automatic downloading with the
command `nxhtml-setup-install'.

For more information about auto download see
`nxhtml-setup-auto-download'."
  (interactive (progn
                 (describe-function 'nxhtml-setup-auto-download)
                 (select-window (get-buffer-window (help-buffer)))
                 (delete-other-windows)
                 ;;(nxhtml-check-convert-to-part-by-part)
                 (list
                  (catch 'command-level
                    (unless (yes-or-no-p "Download all of nXhtml? ")
                      (throw 'command-level nil))
                    (web-vcs-read-nxhtml-dl-dir "Download nXhtml to directory")))))

  (if (not dl-dir)
      (unless (called-interactively-p)
        (error "dl-dir show be a directory"))
  (let ((msg (concat "Downloading nXhtml through Launchpad web interface will take rather long\n"
                     "time (5-15 minutes) so you may want to do it in a separate Emacs session.\n\n"
                     "Do you want to download using this Emacs session? "
                     )))
    (if (not (y-or-n-p msg))
        (message "Aborted")
      (message "")
      (setq message-log-max t)
      (let ((do-byte (y-or-n-p "Do you want to byte compile the files after downloading? ")))
        ;; http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/322
        ;; http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/"
        (nxhtml-download-1 dl-dir nil do-byte)
        )))))


(defun nxhtml-download-1 (dl-dir revision do-byte)
  "Download nXhtml to directory DL-DIR.
If REVISION is nil download latest revision, otherwise the
specified one.

If DO-BYTE is non-nil byte compile nXhtml after download."
  (let* ((has-nxhtml (and (boundp 'nxhtml-install-dir)
                          nxhtml-install-dir))
         (base-url web-vcs-nxhtml-base-url)
         (files-url (concat base-url "files/"))
         ;;(revs-url  (concat base-url "changes/"))
         (rev-part (if revision (number-to-string revision) "head%3A/"))
         (full-root-url (concat files-url rev-part)))
    (when (web-vcs-get-files-from-root 'lp full-root-url dl-dir)
      (web-vcs-set&save-option 'nxhtml-autoload-web nil)
      (when do-byte
        (sit-for 10)
        (web-vcs-message-with-face 'hi-yellow
                                   "Will start byte compilation of nXhtml in new Emacs in 10 seconds")
        (sit-for 10)
        (nxhtmlmaint-start-byte-compilation))
      (let ((autostart-file (expand-file-name "autostart" dl-dir)))
        (unless has-nxhtml (nxhtml-add-loading-to-custom-file autostart-file nil))))))

;; Fix-me: Does not work, Emacs Bug
;; Maybe use wget? http://gnuwin32.sourceforge.net/packages/wget.htm
;; http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=5103
;; (nxhtml-get-release-revision)
(defun nxhtml-get-release-revision ()
  "Get revision number for last release."
  (let* ((all-rev-url "http://code.launchpad.net/%7Enxhtml/nxhtml/main")
         (url-buf (url-retrieve-synchronously all-rev-url))
         (vcs-rec (or (assq 'lp web-vcs-links-regexp)
                      (error "Does not know web-cvs 'lp")))
         (rel-ver-regexp (nth 6 vcs-rec))
         )
    (message "%S" url-buf)
    (with-current-buffer url-buf
      (when (re-search-forward rel-ver-regexp nil t)
        (match-string 1)))))

(defun emacs-Q-no-nxhtml (&rest args)
  (let* ((elp (getenv "EMACSLOADPATH"))
         (elp-list (split-string elp ";"))
         (new-elp-list nil)
         (new-elp "")
         ret)
    (dolist (p elp-list)
      (when (file-exists-p p)
        (let* ((dir (file-name-directory p))
               (last (file-name-nondirectory p))
               (last-dir (file-name-nondirectory
                          (directory-file-name dir))))
          (unless (and (string= "nxhtml" last-dir)
                       (member last '("util" "test" "nxhtml" "related" "alt")))
            (setq new-elp-list (cons p new-elp-list))))))
    (setq new-elp (mapconcat 'identity (reverse new-elp-list) ";"))
    (setenv "EMACSLOADPATH" new-elp)
    (setq ret (apply 'emacs-Q args))
    (setenv "EMACSLOADPATH" elp)
    ret))

;;;;;; Start Testing function
;; (emacs-Q-no-nxhtml "web-vcs.el" "-f" "eval-buffer" "-f" "nxhtml-temp-setup-auto-download")
;; (emacs-Q-no-nxhtml "-l" "c:/test/d27/web-vcs" "-f" "nxhtml-temp-setup-auto-download")
;; (emacs-Q-no-nxhtml "web-vcs.el" "-l" "c:/test/d27/web-autostart.el")
;; (emacs-Q-no-nxhtml "web-vcs.el" "-l" "c:/test/d27/autostart.el")
(defun nxhtml-temp-setup-auto-download ()
  (when (fboundp 'w32-send-sys-command) (w32-send-sys-command #xf030) (sit-for 2))
  (view-echo-area-messages)
  (nxhtml-setup-auto-download "c:/test/d27"))
;;;;;; End Testing function


(provide 'web-vcs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-vcs.el ends here
