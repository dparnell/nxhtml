;;; web-vcs.el --- Download files from lp
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
     "Launchpad uses this"
     ,(rx "href=\""
          (submatch
           (regexp ".*/download/[^\"]*"))
          "\"")
     ,(rx "href=\""
          (submatch
           (regexp ".*%3A/[^\"]*/"))
          "\"")
     "\\([^\/]*\\)$"
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
           ;;(regexp :tag "Subdir regexp")
           ))
  :group 'web-vcs)

;;(nxhtml-download)
;;;###autoload
(defun nxhtml-download ()
  "Download or update nXhtml.
nXhtml is an elisp package, see URL
`http://www.emacswiki.com/NxhtmlMode/'.

After downloading read the instructions in readme.txt in the
download directory for setting up nXhtml.  \(This requires adding
only one line to your .emacs, but you may optionally also byte
compile the files from the nXhtml menu.)"
  (interactive)
  (let ((dl-dir
         (or (when (and (boundp 'nxhtml-install-dir)
                        nxhtml-install-dir)
               (when (yes-or-no-p "Update current nXhtml files? ")
                 nxhtml-install-dir))
             (read-directory-name "Download nXhtml to: ")
             )))
    (web-vcs-get-files-on-page
     'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/"
     t dl-dir
     nil)))

;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/" t "temp" t)
;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/util/" t "temp")
;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/alts/" t "temp")

;;;###autoload
(defun web-vcs-get-files-on-page (web-vcs url recursive dl-dir test)
  (require 'hi-lock) ;; For faces
  (catch 'command-level
    (let* ((vcs-rec (or (assq web-vcs web-vcs-links-regexp)
                        (error "Does not know web-cvs %S" web-vcs)))
           (files-href-regexp  (nth 2 vcs-rec))
           (dirs-href-regexp   (nth 3 vcs-rec))
           (file-name-regexp  (nth 4 vcs-rec)))
      (if (numberp recursive)
          (message "\nSubdir: %S\n" dl-dir)
        ;; Init
        (unless (file-directory-p dl-dir)
          (if (yes-or-no-p (format "Directory %S does not exist, create it? " (expand-file-name dl-dir)))
              (mkdir dl-dir t)
            (message "Can't download then")
            (throw 'command-level nil)))
        (let ((old-win (selected-window)))
          (switch-to-buffer-other-window "*Messages*")
          (goto-char (point-max))
          (insert "\n")
          (insert (propertize (format "\n\nDownload: %S\n" url) 'face 'hi-gold))
          (insert "\n")
          (set-window-point (selected-window) (point-max))
          (select-window old-win)))
      ;;(throw 'command-level nil)
      (let* ((url-buf (url-retrieve-synchronously url))
             files
             suburls
             (temp-file (expand-file-name "temp.tmp" dl-dir)))
        (with-current-buffer url-buf
          (goto-char (point-min))
          (unless (looking-at "HTTP/.* 200 OK\n")
            (switch-to-buffer url-buf)
            (error "Download error: %S" url))
          (unless (file-directory-p dl-dir)
            (make-directory dl-dir t))
          (goto-char (point-min))
          (while (re-search-forward
                  files-href-regexp
                  nil t)
            (add-to-list 'files (match-string 1)))
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
                  (message "file-url=%S" file-url)
                  (message "file-name=%S" file-name)
                  (message "file-dl-name=%S" file-dl-name)
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
            ))
        (when suburls
          (dolist (suburl suburls)
            (let ((dir (substring suburl (length url))))
              (web-vcs-get-files-on-page web-vcs
                                         suburl
                                         (if (numberp recursive)
                                             (1+ recursive)
                                           1)
                                         (expand-file-name dir dl-dir)
                                         test))))
        )))
  (unless (numberp recursive)
    (message "Download is ready. Old non-matching files renamed to *.moved."))
  )

(provide 'web-vcs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-vcs.el ends here
