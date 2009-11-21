;; -*- emacs-lisp -*-
;; License: Gnu Public License
;;
;; Additional functionality that makes flymake error messages appear
;; in the minibuffer when point is on a line containing a flymake
;; error. This saves having to mouse over the error, which is a
;; keyboard user's annoyance

;; This code started from an idea in a paste.

;;flymake-ler(file line type text &optional full-file)
(defun flymakemsg-show-err-at-point ()
  (condition-case err
      (flymakemsg-show-err-at-point-1)
    (error (message "%s" err))))

(defun flymakemsg-show-err-at-point-1 ()
  "If point is on a flymake error, show it in echo area."
  (interactive)
  ;; (let ((line-no (line-number-at-pos)))
  ;;   (dolist (elem flymake-err-info)
  ;;     (if (eq (car elem) line-no)
  ;;         (let ((err (car (second elem))))
  ;;           (message "%s" (fly-pyflake-determine-message err))))))
  (let ((flyovl (catch 'errovl
                  (dolist (ovl (overlays-at (point)))
                    (when (eq 'flymake-errline (overlay-get ovl 'face))
                      (throw 'errovl ovl))))))
    (when flyovl (message "%s" (propertize
                                (overlay-get flyovl 'help-echo)
                                'face 'flymake-errline)))))

(defadvice flymake-mode (after
                         flymakemsg-ad-flymake-mode
                         activate compile)
  "Turn on showing of flymake errors then point is on them.
This shows the error in the echo area."
  (if flymake-mode
      (add-hook 'post-command-hook 'flymakemsg-show-err-at-point t t)
    (remove-hook 'post-command-hook 'flymakemsg-show-err-at-point t)))

;;; I have no idea what this does.
;; (defun fly-pyflake-determine-message (err)
;;   "pyflake is flakey if it has compile problems, this adjusts the
;; message to display, so there is one ;)"
;;   (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
;; 	((null (flymake-ler-file err))
;; 	 ;; normal message do your thing
;; 	 (flymake-ler-text err))
;; 	(t ;; could not compile err
;; 	 (format "compile error, problem on line %s" (flymake-ler-line err)))))

