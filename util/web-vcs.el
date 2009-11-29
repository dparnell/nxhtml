;;; web-vcs.el --- Download file trees from VCS web pages
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-11-26 Thu
(defconst web-vcs:version "0.5") ;; Version:
;; Last-Updated: 2009-11-27 Fri
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
;; See the example `nxhtml-download'.
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

(defcustom web-vcs-links-regexp
  `(
    (lp
     "http://www.launchpad.com/ uses this 2009-11-29\nwith Loggerhead 1.10, generic?"
     ,(rx "href=\""
          (submatch
           (regexp ".*/download/[^\"]*"))
          "\"")
     ,(rx "href=\""
          (submatch
           (regexp ".*%3A/[^\"]*/"))
          "\"")
     "\\([^\/]*\\)$"
     ,(rx "for revision"
          (+ whitespace)
          "<span>"
          (submatch (+ digit))
          "</span>")
     ;;"/head%3A/\\([^/]*\\)/$"
     )
    )
  "Regexp pattern for matching links on a VCS web page.
It is always sub match 1 from these patterns that are used."
  :type '(repeat
          (list
           (symbol :tag "VCS web system type identifier")
           (string :tag "Description")
           (regexp :tag "Files URL regexp")
           (regexp :tag "Dirs URL regexp")
           (regexp :tag "File name URL part regexp")
           (regexp :tag "Revision regexp")
           ;;(regexp :tag "Subdir regexp")
           ))
  :group 'web-vcs)

;;(nxhtml-download)
;;;###autoload
(defun nxhtml-download (dl-dir)
  "Download or update nXhtml.
nXhtml is an elisp package, see URL
`http://www.emacswiki.com/NxhtmlMode/'.

After downloading read the instructions in readme.txt in the
download directory for setting up nXhtml.  \(This requires adding
only one line to your .emacs, but you may optionally also byte
compile the files from the nXhtml menu.)"
  (interactive
   (list (or (when (and (boundp 'nxhtml-install-dir)
                        nxhtml-install-dir
                        (yes-or-no-p "Update current nXhtml files? "))
               nxhtml-install-dir)
             (read-directory-name "Download nXhtml to: "))))
  (web-vcs-get-files-from-root
   'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/"
   dl-dir))

;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/" t "c:/test/temp13/" t)
;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/util/" t "temp" t)
;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/alts/" t "temp" t)

;;;###autoload
(defun web-vcs-get-files-from-root (web-vcs url dl-dir)
  (web-vcs-get-files-on-page web-vcs url t (file-name-as-directory dl-dir) nil))

(defun web-vcs-get-files-on-page (web-vcs url recursive dl-dir test)
  (require 'hi-lock) ;; For faces
  (unless (string= dl-dir (file-name-as-directory (expand-file-name dl-dir)))
    (error "dl-dir=%S must be a full directory path" dl-dir))
  (catch 'command-level
    (let* ((vcs-rec (or (assq web-vcs web-vcs-links-regexp)
                        (error "Does not know web-cvs %S" web-vcs))))
      (unless (file-directory-p dl-dir)
        (if (yes-or-no-p (format "Directory %S does not exist, create it? " (expand-file-name dl-dir)))
            (mkdir dl-dir t)
          (message "Can't download then")
          (throw 'command-level nil)))
      (let ((old-win (selected-window)))
        (switch-to-buffer-other-window "*Messages*")
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize (format "\n\nWeb-Vcs Download: %S\n" url) 'face 'hi-gold))
        (insert "\n")
        (redisplay t)
        (set-window-point (selected-window) (point-max))
        (select-window old-win))
      (setq web-vcs-num-moved 0)
      (let* ((rev-file (expand-file-name "web-vcs-revision.txt"))
             (rev-buf (find-file-noselect rev-file))
             (old-revision (with-current-buffer rev-buf
                             (save-restriction
                               (widen)
                               (buffer-substring-no-properties (point-min) (point-max)))))
             (ret (web-vcs-get-files-on-page-1
                   vcs-rec url (if recursive 0 nil) dl-dir old-revision nil test))
             (dl-revision (nth 0 ret))
             (moved       (nth 1 ret)))
        (with-current-buffer rev-buf
          (save-restriction
            (widen)
            (delete-region (point-min) (point-max))
            (insert dl-revision)
            (basic-save-buffer)
            (kill-buffer)))
        (message "Download is ready. %i old non-matching files renamed to *.moved." moved)
        ))))

(defun web-vcs-get-files-on-page-1 (vcs-rec url recursive dl-dir old-revision dl-revision test)
  (let* ((files-href-regexp  (nth 2 vcs-rec))
         (dirs-href-regexp   (nth 3 vcs-rec))
         (file-name-regexp   (nth 4 vcs-rec))
         (revision-regexp    (nth 5 vcs-rec))
         (url-buf (url-retrieve-synchronously url))
         this-dl-revision
         files
         suburls
         (moved 0)
         (temp-file (expand-file-name "temp.tmp" dl-dir)))
    (with-current-buffer url-buf
      (goto-char (point-min))
      (unless (looking-at "HTTP/.* 200 OK\n")
        (switch-to-buffer url-buf)
        (error "Download error: %S" url))
      (unless (file-directory-p dl-dir)
        (make-directory dl-dir t))
      (goto-char (point-min))
      (if (not (re-search-forward revision-regexp nil t))
          (progn
            (message (propertize "Can't find revision number on %S" 'face 'hi-salmon) url)
            (throw 'command-level nil))
        (setq this-dl-revision (match-string 1))
        (if dl-revision
            (unless (string= dl-revision this-dl-revision)
              (message (propertize "Revision on %S is %S, but should be %S" 'face 'hi-salmon)
                       url this-dl-revision dl-revision)
              (throw 'command-level nil))
          (when (string= this-dl-revision old-revision)
            (message (propertize "You already got revision %s" 'face 'hi-yellow)
                     this-dl-revision)
            (throw 'command-level nil))))
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
    (dolist (file (reverse files))
      (let* ((file-url file)
             (file-name (progn
                          (when (string-match file-name-regexp file-url)
                            (match-string 1 file-url))))
             (file-dl-name (expand-file-name file-name dl-dir))
             temp-buf)
        (if test
            (progn
              (message "TEST file-url=%S" file-url)
              (message "TEST file-name=%S" file-name)
              (message "TEST file-dl-name=%S" file-dl-name)
              )
          (while (setq temp-buf (find-buffer-visiting temp-file))
            (set-buffer-modified-p nil)
            (kill-buffer temp-buf))
          ;; Use url-copy-file, this takes care of coding system.
          (url-copy-file file-url temp-file t)
          (let* ((new-buf (find-file-noselect temp-file))
                 (new-src (with-current-buffer new-buf
                            (save-restriction
                              (widen)
                              (buffer-substring-no-properties (point-min) (point-max)))))
                 (old-exists (file-exists-p file-dl-name))
                 (old-buf-open (find-buffer-visiting file-dl-name))
                 (old-buf (or old-buf-open
                              (when old-exists
                                (let ((auto-mode-alist nil))
                                  (find-file-noselect file-dl-name)))))
                 old-src)
            (when old-buf
              (with-current-buffer old-buf
                (when (buffer-modified-p)
                  (save-excursion
                    (switch-to-buffer old-buf)
                    (if (y-or-n-p (format "Buffer %S is modified, save to make a backup? "
                                          file-dl-name))
                        (save-buffer)
                      (set-buffer-modified-p nil))))
                (setq old-src (save-restriction
                                (widen)
                                (buffer-substring-no-properties (point-min) (point-max))))))
            (if (and old-src (string= new-src old-src))
                (message (propertize "File %S was ok" 'face 'hi-green) file-dl-name)
              (when old-exists
                (let ((backup (concat file-dl-name ".moved")))
                  (when (file-exists-p backup)
                    (delete-file backup))
                  (rename-file file-dl-name backup)))
              (rename-file temp-file file-dl-name)
              (if old-exists
                  (message (propertize "Updated %S" 'face 'hi-yellow) file-dl-name)
                (message (propertize "Downloaded %S" 'face 'hi-green) file-dl-name))
              (when old-buf-open
                (with-current-buffer old-buf-open
                  (revert-buffer))))
            (redisplay t)
            (sit-for 2)
            (unless old-buf-open
              (when old-buf
                (kill-buffer old-buf)))))
        (redisplay t)))
    (when suburls
      (dolist (suburl suburls)
        (let* ((dl-sub-dir (substring suburl (length url)))
               (full-dl-sub-dir (file-name-as-directory
                                 (expand-file-name dl-sub-dir dl-dir))))
          (unless (web-vcs-contains-file dl-dir full-dl-sub-dir)
            (error "Subdir %S not in %S" dl-sub-dir dl-dir))
          (let* ((ret (web-vcs-get-files-on-page-1 vcs-rec
                                                  suburl
                                                  (1+ recursive)
                                                  full-dl-sub-dir
                                                  old-revision
                                                  this-dl-revision
                                                  test)))
            (setq moved (+ moved (nth 1 ret)))
            ))))
    (list this-dl-revision moved)
    ))

(defun web-vcs-contains-file (dir file)
  (assert (string= dir (file-name-as-directory (expand-file-name dir))) t)
  (assert (or (string= file (file-name-as-directory (expand-file-name file)))
              (string= file (expand-file-name file))) t)
  (let ((dir-len (length dir)))
    (assert (string= "/" (substring dir (1- dir-len))))
    (when (> (length file) dir-len)
      (string= dir (substring file 0 dir-len)))))


(provide 'web-vcs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-vcs.el ends here
