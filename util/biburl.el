;;; biburl.el --- Get bibiolgraphic references for web pages
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
;; 
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

;; (require 'web-vcs) ;; autoloaded

;; (biburl-get-page "http://www.ncbi.nlm.nih.gov/pubmed/17501969")
;; (biburl-get-page "doi:10.1186/1744-859X-8-2")
(defun biburl-get-page (url)
  (when (string= (substring url 0 4)
                 "doi:")
    (setq url (concat "http://dx.doi.org/" url)))
  (let* ((buf-res (web-vcs-url-retrieve-synch url))
         (buf (car buf-res))
         (res (cdr buf-res)))
    (unless buf
      (error "status=%S" res))
    ;;(switch-to-buffer-other-window buf)
    buf))

;; (biburl-get-data "http://www.ncbi.nlm.nih.gov/pubmed/17501969")
;; (biburl-get-data "doi:10.1186/1744-859X-8-2")
(defun biburl-get-data (url)
  (let ((buf (biburl-get-page url))
        authors
        year
        title
        journal
        volume
        issue
        firstpage
        lastpage
        doi
        pmid
        section
        )
    (with-current-buffer buf
      (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
        (goto-char (point-min))
        (let ((no-authors (unless authors t)))
          (while (re-search-forward
                  "<meta +name *= *\"\\(dc\.[^\"]*\\)\" +content *= *\"\\([^\"]*\\)\"" nil t)
            (let ((mn (match-string-no-properties 1))
                  (mc (match-string-no-properties 2)))
              (cond
               ((when no-authors (string= mn "dc.creator"))
                (let* ((mclist (split-string mc " *" t))
                       (first  (nth 0 mclist))
                       (last   (nth 1 mclist)))
                  (setq authors (cons (list first last) authors))))
               ((string= mn "dc.date") (setq year (substring-no-properties mc 0 4)))
               ((string= mn "dc.title") (setq title mc))
               ((string= mn "dc.source")
                ;; (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" "where 2010 5:87")
                (when (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" mc)
                  (setq journal   (match-string 1 mc))
                  (setq year      (match-string 2 mc))
                  (setq volume    (match-string 3 mc))
                  (setq firstpage (match-string 4 mc))))
               ((string= mn "dc.identifier") (setq doi mc))
               )))
          (when no-authors (setq authors (reverse authors)))))
      (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
        (goto-char (point-min))
        (while (re-search-forward "<meta +name *= *\"\\([^\"]*\\)\" +content *= *\"\\([^\"]*\\)\"" nil t)
          (let ((mn (match-string-no-properties 1))
                (mc (match-string-no-properties 2)))
            (cond
             ((string= mn "citation_authors")
              (let ((mclist (split-string mc ", *" t)))
                (setq authors (mapcar (lambda (a)
                                        (split-string a " +" t))
                                      mclist))))
             ((string= mn "citation_year") (setq year mc))
             ((string= mn "citation_title") (setq title mc))
             ((string= mn "citation_journal_title") (setq journal mc))
             ((string= mn "citation_volume") (setq volume mc))
             ((string= mn "citation_issue") (setq issue mc))
             ((string= mn "citation_firstpage") (setq firstpage mc))
             ((string= mn "citation_lastpage") (setq lastpage mc))
             ((string= mn "citation_doi") (setq doi mc))
             ))))
      (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
        (goto-char (point-min))
        (setq authors nil) ;; fix-me
        (when (search-forward "<rdf:RDF " nil t)
          (let ((beg (point))
                (end (search-forward "</rdf:RDF>"))
                (no-authors (unless authors t))
                )
            (goto-char beg)
            (while (re-search-forward "<\\([^/>]+\\)>\\([^<]+\\)<" end t)
              (let ((rf (match-string-no-properties 1))
                    (rv (match-string-no-properties 2)))
                ;;(message "rf=%S, rv=%S" rf rv)
                (cond
                 ((string= rf "dc:title") (setq title rv))
                 ((when no-authors (string= rf "dc:creator"))
                  (let* ((names (split-string rv ", *"))
                         (firstname (nth 1 names))
                         (lastname  (nth 0 names)))
                    (setq authors (cons (list firstname lastname) authors))))
                 ((string= rf "dc:identifier")
                  (cond
                   ((string= "info:doi/" (substring-no-properties rv 0 9))
                    (setq doi (substring-no-properties rv 9)))
                   ((string= "info:pmid/" (substring-no-properties rv 0 10))
                    (setq pmid (substring-no-properties rv 10)))
                   (t (error "Unknown dc:identifier=%S" rv))))
                 ((string= rf "dc:date") (setq year (substring-no-properties rv 0 4)))
                 ((string= rf "prism:publicationName") (setq journal rv))
                 ((string= rf "prism:publicationDate") (setq year (substring-no-properties rv 0 4)))
                 ((string= rf "prism:volume") (setq volume rv))
                 ((string= rf "prism:number") (setq issue rv)) ;; fix-me: Is this correct?
                 ((string= rf "prism:startingPage") (setq firstpage rv))
                 ((string= rf "prism:endingPage") (setq lastpage rv)) ;; Fix-me: Is this correct?
                 )))
            (when no-authors (setq authors (reverse authors)))
            )))
        ) ;; buf
    (list
     :authors authors
     :year year
     :title title
     :journal journal
     :volume volume
     :issue issue
     :firstpage firstpage
     :lastpage lastpage
     :doi doi
     :pmid pmid)
     ))

;; fix-me: do not understand where the data is:
;;   (biburl-copy-ref "http://www.springerlink.com/content/qh290kr305158620/")
;;   (biburl-copy-ref "http://www.ncbi.nlm.nih.gov/pubmed/17501969")
;; These are OK:
;;   (biburl-copy-ref "doi:10.1186/1744-859X-8-2")
(defun biburl-copy-ref (url)
  (interactive (list (read-string "URL: ")))
  (let* ((data (biburl-get-data url))
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

(provide 'biburl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; biburl.el ends here
