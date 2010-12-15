;;; biburl.el --- bibiolgraphic references for web pages etc
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-11-29 Mon
;; Version: 0.1
;; Last-Updated: 2010-11-30 Tue
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
;; See `biburl-copy-ref'.
;; See `biburl-apa2elin'.
;;
;; I am not sure this is really useful. It is just a test so far.
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

;; fix-me: do not understand where the data is:
;;   (biburl-copy-ref "http://www.springerlink.com/content/qh290kr305158620/")
;; http://www.springerlink.com/export.mpx?code=qh290kr305158620&mode=ris
;;   (biburl-copy-ref "http://www.ncbi.nlm.nih.gov/pubmed/17501969")
;;   http://www.springerlink.com/content/j5r1t50432n8g247/
;;   http://www.springerlink.com/export.mpx?j5r1t50432n8g247&mode=ris
;; These are OK:
;;   (biburl-copy-ref "doi:10.1186/1744-859X-8-2")

;;;###autoload
(defun biburl-copy-ref (url)
  (interactive (list (read-string "URL: ")))
  (let* ((data (bibhlp-get-data-from-url url))
         (authors (plist-get data :authors))
         (year (plist-get data :year))
         (title (plist-get data :title))
         (journal (plist-get data :journal))
         (volume (plist-get data :volume))
         (issue (plist-get data :issue))
         (firstpage (plist-get data :firstpage))
         (lastpage (plist-get data :lastpage))
         (doi (plist-get data :doi))
         missing
         text
         (ref-format "APA"))
    ;; APA
    (if (not authors)
        (push "authors" missing)
      (while authors
        (let* ((a (pop authors))
               (first (nth 0 a))
               (last  (nth 1 a)))
          (when text (setq text (concat text ", ")))
          (unless authors (setq text (concat text "& ")))
          (setq text (concat text last ", " (substring first 0 1) ".")))))

    (if (not year)
        (push "year" missing)
      (setq text (concat text " (" year ")")))
    (setq text (concat text "."))

    (if (not title)
        (push "title" missing)
      (setq text (concat text " " title)))
    (setq text (concat text "."))

    (if (not journal)
        (push "journal" missing)
      (setq text (concat text " " journal)))
    (setq text (concat text ","))

    (if (not volume)
        (push "volume" missing)
      (setq text (concat text " " volume)))
    (if (not issue)
        (push "issue" missing)
      (setq text (concat text "(" issue ")")))
    (setq text (concat text ","))

    (if (not firstpage)
        (push "firstpage" missing)
      (setq text (concat text " " firstpage)))
    (if (not lastpage)
        (push "lastpage" missing)
      (setq text (concat text "-" lastpage)))
    (setq text (concat text "."))

    (if (not doi)
        (push "doi" missing)
      (setq text (concat text " doi:" doi)))
    (setq text (concat text "\n"))

    (if (not (string-match-p "[a-zA-Z0-9]" text))
        (message "Sorry, could not get any reference data")
      (kill-new text)
      (message text)
      (let ((msg (format "Copied %s ref format to clipboard" ref-format)))
        (when missing (setq msg (concat msg (format " - Missing fields: %S" missing))))
        (message "%s" msg)))
    nil
    ))

;;;###autoload
(defun org-copy-url-ref-at-point ()
  (interactive)
  (let* ((link (org-copy-url-at-point))
         )
    (when link
      (message "Trying to get reference data...")
      (biburl-copy-ref link))))


(when t ;; fix-me
  (org-defkey org-mouse-map [(control ?c) ?l] 'org-copy-url-at-point)
  (org-defkey org-mouse-map [(control ?c) ?r] 'org-copy-url-ref-at-point)
  (define-key my-keys-mode-map [(control ?c) ?s] 'biburl-apa2elin)
  )

;;;###autoload
(defun biburl-apa2elin (beg end)
  ;; http://refformer.com/
  (interactive "r")
  (let (sentence-end-double-space
        (here (point))
        (old-mark-active mark-active)
        (old-beg (region-beginning))
        (old-end (region-end))
        beg-yy end-yy yy
        authors
        txt
        )
    (unless (and mark-active beg end (< beg end))
      (backward-paragraph)
      (back-to-indentation)
      (setq beg (point))
      (forward-paragraph)
      (setq end (point)))
    ;; Find year in slightly different formats.
    (goto-char beg)
    (if (re-search-forward "(\\([0-9]\\{4\\}\\))[.:]?" end t)
        (progn
          (setq beg-yy (match-beginning 0))
          (setq end-yy (match-end 0))
          (setq yy (match-string-no-properties 1)))
      (setq beg-yy end)
      (setq end-yy end))
    (goto-char beg)
    ;; Get authors. Formats we try to cover are:
    ;;   Saylam, C., Ucerler, H., Kitis, O., Ozand, E. and Gonul, A.S. (2006)
    ;;   Cooper P, Murray L, Wilson A, Romaniuk H (2003).
    ;;   Glezer I, Simard AR, Rivest S (2007):
    (let ((re-author "\\([^\w]+\\),?[\w]+\\([^,\w]+\\),"))
      (while ( < (point) beg-yy)
        (let ((b1 (point))
              e1
              who
              lastname initials)
          (if (not (re-search-forward re-author beg-yy t))
              (goto-char beg-yy)
            (setq lastname (match-string-no-properties 1))
            (setq initials (match-string-no-properties 2))
            (setq initials (delete ?. (append initials nil)))
            (push (cons lastname initials) authors)))))
    (dolist (author authors)
      (let ((lastname (car author)))
        (when txt (setq txt (concat txt " AND ")))
        (setq txt (concat txt "au:" lastname))))
    ;;(when txt (setq txt (concat txt " AND ")))
    ;;(setq who (buffer-substring-no-properties b1 e1))
    ;;(when (string-match-p " " who)
    ;;  (setq who (concat "\"" who "\"")))
    ;;(setq txt (concat txt "au:" who))
    ;; Get title, journal, volume, issue, pages.
    ;; Formast we try to cover are:
    ;;   Controlled trial. I. Impact on maternal mood. British Journal Psychiatry 182, 412–419
    ;;   Focal gray: A follow-up study. Neuropsychopharmacology 32:2057–2066.
    ;;   Reduced hippocampal. Surg. Radiolog. Anat., 28: 82–87.
    ;;   Microglia act: a (R)-[11C]PK11195 study. Biol Psych, 64(9), 820-822.
    ;;
    ;; All possibly followed by doi:, pmcid:, pmid: etc.
    (let ((re-ti-jo-vo-is-pg "[0-9][,:]\w*[0-9]+\\(?:-[0-9]+\\).?$")
          (re-inds "\b[^:]+:[^\w]+\w+"))

      (unless (eq beg-yy end)
        (goto-char beg-yy)
        (goto-char (search-forward ")." end t))
        (skip-syntax-forward " ")
        (let ((b1 (point))
              e1 ti tw)
          (re-search-forward "[.?!:]" end t)
          (when (> (point) b1)
            (setq e1 (point))
            (setq ti (buffer-substring-no-properties b1 e1))
            (setq tw (split-string ti "[][ \f\t\n\r\v!.:,()-]" t))
            (dolist (w tw)
              (when (< 7 (length w))
                (when txt (setq txt (concat txt " AND ")))
                (setq txt (concat txt "ti:" w)))))))
      (message "txt=%S" txt)
      (kill-new txt)
      (browse-url (concat "http://elin.lub.lu.se.ludwig.lub.lu.se/elin?func=advancedSearch&lang=se&query="
                          (browse-url-encode-url txt)))
      (goto-char here))))

  ;; GET http://elin.lub.lu.se.ludwig.lub.lu.se/elin
  ;; fromYear	1900
  ;; func	advancedSearch
  ;; lang	se
  ;; pubYear	allYears
  ;; query	au:bola and au:lehtinen and au:cullberg
  ;; submitButtonName	Sök
  ;; toYear	2011
  ;; (browse-url-encode-url "?name=val")

  ;; Installing the ParsCit ruby client from http://aye.comp.nus.edu.sg/parsCit/:
  ;; - The w32 version does not work, cygwin must be used
  ;;
  ;; - Installing RubyGems in Cygwin
  ;;     http://stevenharman.net/blog/archive/2008/11/12/installing-rubygems-in-cygwin.aspx
  ;;   download the rubygems tarball from ruby forge unpack the tarball
  ;;   in a bash terminal, navigate to the unpacked directory run the
  ;;   following command:
  ;;     - ruby setup.rb install
  ;;   update rubygems by running the following:
  ;;     - gem update --system
  ;;   note: you may need to run the updated command twice if you have
  ;;   any previously installed gems.
  ;;
  ;; - gem install soap4r
  ;; - gem install rake
  ;; - gem install libxml-ruby ;; needed?
  ;; - gem install xmlparser

  ;; Local Variables:
  ;; coding: utf-8
  ;; End:

  (provide 'biburl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; biburl.el ends here
