;;; win-alg.el --- Window size computation
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-08-12 Wed
;; Version: 0.21
;; Last-Updated: 2010-06-14 Mon
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window creation, basic setters/getters etc

(defun wa-make-window (name parent size wumin wumax real-window)
  (list (list 'id name parent (wa-name parent)) ;; Easier debugging + parent
        (list 'wchild nil)                      ;; Child windows list
        (list 'wusr-size size wumin wumax)      ;; Current size and restrictions: wumin, wumax
        (list 'wreq-size nil nil nil)           ;; Slot for computing requirements: wrmin wrmax wfixed
        (list 'wset-size nil nil)               ;; Slot for new size:
        ;; result-flag, wset Fix-me: add height - maybe better just to
        ;; run this two times? Why not
        real-window
        ))

;; Fix-me: Maybe make defmacro to make those getters setters... -
;; including checks...

;; Fix-me: add arg for width/height

;;; Getters
(defun wa-name      (window) (nth 1 (nth 0 window))) ;; 'id
(defun wa-parent    (window) (nth 2 (nth 0 window))) ;; 'id
;; Current
(defun wa-child     (window) (nth 1 (nth 1 window))) ;; 'wchild
(defun wa-wucur     (window) (nth 1 (nth 2 window))) ;; 'wusr-size
(defun wa-wumin     (window) (nth 2 (nth 2 window))) ;; 'wusr-size
(defun wa-wumax     (window) (nth 3 (nth 2 window))) ;; 'wusr-size
;; Computation
(defun wa-wrmin     (window) (nth 1 (nth 3 window))) ;; 'wreq-size
(defun wa-wrmax     (window) (nth 2 (nth 3 window))) ;; 'wreq-size
(defun wa-wfixed    (window) (nth 3 (nth 3 window))) ;; 'wreq-size
;; Result
(defun wa-wres-flag (window) (nth 1 (nth 4 window))) ;; 'wset-size
(defun wa-wset      (window) (nth 2 (nth 4 window))) ;; 'wset-size

;;; Setters
(defun wa-set-name      (window name)  (setcar (nthcdr 1 (nth 0 window)) name))  ;; 'id
(defun wa-set-child     (window child) (setcar (nthcdr 1 (nth 1 window)) child)) ;; 'wchild
;; Current
(defun wa-set-wucur     (window wucur) (setcar (nthcdr 1 (nth 2 window)) wucur)) ;; 'wusr-size
(defun wa-set-wumin     (window wumin) (setcar (nthcdr 2 (nth 2 window)) wumin)) ;; 'wusr-size
(defun wa-set-wumax     (window wumax) (setcar (nthcdr 3 (nth 2 window)) wumax)) ;; 'wusr-size
;; Computation
(defun wa-set-wrmin     (window wrmin) (setcar (nthcdr 1 (nth 3 window)) wrmin)) ;; 'wreq-size
(defun wa-set-wrmax     (window wrmax) (setcar (nthcdr 2 (nth 3 window)) wrmax)) ;; 'wreq-size
(defun wa-set-wfixed    (window wrfix) (setcar (nthcdr 3 (nth 3 window)) wrfix)) ;; 'wset-size
;; Result
(defun wa-set-wres-flag (window flag)  (setcar (nthcdr 1 (nth 4 window)) flag))  ;; 'wset-size
(defun wa-set-wset      (window size)  (setcar (nthcdr 2 (nth 4 window)) size))  ;; 'wset-size

(defun wa-set-child-windows (parent vertical &rest sizes)
  (unless wa-failed (assert (< 1 (length sizes)) t))
  (let (children
        (num 0))
    (setq children (mapcar (lambda (size)
                             (setq num (1+ num))
                             (if vertical
                                 (wa-make-window (format "%s-%d" (wa-name parent) num nil)
                                                 parent
                                                 nil
                                                 (nth 0 size)
                                                 (nth 1 size))))
                           sizes))
    (wa-set-child parent children)
    parent))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Errors

;; Fix-me: remove this...
(defvar wa-failed nil)

(defun wa-error (format-string &rest args)
  (setq wa-failed t)
  (apply 'message (propertize format-string 'face 'secondary-selection)
         args)
  (throw 'top-level nil))

(defun wa-win-error (win format-string &rest args)
  (wa-set-wres-flag win (concat "FAILED: " (apply 'format format-string args)))
  ;;(apply 'wa-error format-string args)
  (throw 'win-error (apply 'format format-string args)))

(defun wa-did-not-fit (win)
  ;; Fix-me: throw both value diff and message. Any ambiguity in the
  ;; value diff?
  (catch 'win-error
    (let* ((wumin (wa-wumin win))
           (wumax (wa-wumax win))
           (wrmin (wa-wrmin win))
           (wrmax (wa-wrmax win))
           (wset  (wa-wset win))
           (wfix  (wa-wfixed win))
           (sf (if wfix "fix" "set")))
      (wa-set-wres-flag win 'FAILED)
      ;; Top window
      (when (and wset wrmin)
        (unless (<= wrmin wset)
          (wa-win-error win "Window %s %s size too small=%d, min=%d" (wa-name win) sf wset wrmin)))
      (when (and wset wrmax)
        (unless (>= wrmax wset)
          (wa-win-error win "Window %s %s size too large=%d, max=%s" (wa-name win) sf wset wrmax)))
      ;; All
      (when (and wumax wrmin)
        (unless (<= wrmin wumax)
          (wa-win-error win "Window %s too small, min=%d, but can be max=%d" (wa-name win) wrmin wumax)))
      (when (and wrmax wumin)
        (unless (>= wrmax wumin)
          (wa-win-error win "Window %s's childs too small, max=%d, but can be min=%d" (wa-name win) wrmax wumin)))
      (wa-set-wres-flag win 'OK))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Computation of sizes

(defun wa-clear-computed (win)
  (wa-set-wrmin win nil)
  (wa-set-wrmax win nil)
  (wa-set-wset  win nil)
  (dolist (c (wa-child win))
    (wa-clear-computed c)))

(defun wa-init-fail-flag (win)
  (wa-set-wres-flag win 'INIT)
  (dolist (c (wa-child win))
    (wa-init-fail-flag c)))

(defun wa-compute-required-to-message (win)
  (catch 'wa-fit-error
    (wa-compute-required win)))

;; Fi-me: Remember that the children are actually two steps down in
;; the tree, since every second tree level is split horizontally and
;; those in between vertically. This must be added to
;; `wa-compute-requied' and `wa-compute-resulting'.
(defun wa-compute-required (win)
  "Walk up from window WIN collecting needed sizes.
Throw string message to 'wa-fit-error if does not fit."
  (let ((childs (wa-child win))
        (wumin (wa-wumin win))
        (wumax (wa-wumax win))
        (cmin 0)
        (cmax -1)
        (can-fit t))
    (if (not childs)
        (setq cmax nil)
      ;; Clear childes set sizes.
      (dolist (c childs)
        (wa-set-wset c nil))
      (dolist (c childs)
          (let* ((res (wa-compute-required c))
                 (res-min (nth 0 res))
                 (res-max (nth 1 res)))
            ;; Fix-me: Use emacs window min.
            (unless res-min (setq res-min 1)) ;; Fix-me: emacs window min
            ;; If fixed size just set MAX and MIN to desired size.
            (when (wa-wfixed c)
              (setq res-min (wa-wfixed c))
              (setq res-max (wa-wfixed c)))
            ;; Just sum the MIN.
            (setq cmin (+ cmin res-min))
            ;; Check MAX
            (if (and res-max cmax)
                ;; ... ok, let us sum MAX to see how big we can be ...
                (if (> cmax 0)
                    (setq cmax (+ cmax res-max))
                  ;; First time:
                  (setq cmax res-max))
              ;; Hurray, at least one child can grow!
              (setq cmax nil))))
      ;; Sanity. Fix-me: use emacs window min.
      (unless wa-failed (assert (<= (* 1 (length childs)) cmin) t))
      (unless wa-failed (assert (or (not cmax) (<= (* 1 (length childs)) cmax)) t)))
    (when wumin (setq cmin (max wumin (or cmin wumin))))
    (when wumax (setq cmax (min wumax (or cmax wumax))))
    (wa-set-wrmin win cmin)
    (wa-set-wrmax win cmax)
    (let ((did-not-fit (wa-did-not-fit win)))
      (if did-not-fit
          (throw 'wa-fit-error did-not-fit)
        (list (wa-wrmin win)
              (wa-wrmax win))))))

(defun wa-compute-resulting (win strategy)
  "Walk down compute resulting sizes and apply them."
  ;; NOTE: This is the part that can tie into the C functions. This
  ;; computes the sizes to apply level by level when going down.
  ;;
  ;; To apply it to the C level I suggest implementing a function in C
  ;; that accept a list of sizes, one size per window on that
  ;; level. Walk the C structures in parallel with this when applying
  ;; the sizes. (I do not think it is necessary to have this code in
  ;; C.)
  (when (wa-child win)
    (let ((cmin   (wa-wrmin  win))
          (cmax   (wa-wrmax  win))
          (width  (wa-wset win))
          (childs (wa-child win)))
      (case strategy
        ('eq-sizes
         (let ((rest-width width)
               (goal (/ width (length childs)))
               (rest-childs (copy-sequence childs)))
           ;; Clear childes
           (dolist (c childs) (wa-set-wset c nil))
           ;; Check child min requirements
           (dolist (c (copy-sequence rest-childs))
             (let ((wrmin (wa-wrmin c)))
               (when (and wrmin (<= goal wrmin))
                 (wa-set-wset c (wa-wrmin c))
                 (setq rest-childs (delete c rest-childs))
                 (setq rest-width (- rest-width (wa-wrmin c))))))
           (setq goal (/ rest-width (length childs)))
           ;; Check child max requirements
           (dolist (c (copy-sequence rest-childs))
             (let ((wrmax (wa-wrmax c)))
               (when (and wrmax (>= goal wrmax))
                 (wa-set-wset c (wa-wrmax c))
                 (setq rest-childs (delete c rest-childs))
                 (setq rest-width (- rest-width (wa-wrmax c))))))
           (setq goal (/ rest-width (length childs)))
           ;; Distribute the rest, taking care of rounding
           (wa-set-wset (car rest-childs)
                        (- rest-width (* goal (1- (length rest-childs)))))
           (dolist (c (cdr rest-childs))
             (wa-set-wset c goal))))
        (t (wa-error "Unknown strategy: %s" strategy)))
      ;; Check
      (let ((w 0))
        (dolist (c childs)
          (let ((wset (wa-wset c)))
            (unless wa-failed (assert (<= 0 wset) t))
            (setq w (+ w wset))))
        (unless (= w (wa-wset win))
          (wa-error "Bad set sizes child sum w=%d, win width=%d" w (wa-wset win))))
      ;; Call the suggested C level function here for example.
      ;; .......
      ;; Walk down
      (dolist (c childs)
        (wa-compute-resulting c strategy))))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example of new implementation resizing functions

(defun wa-siblings (window)
  (let* ((all (wa-child (wa-parent window)) )
         (me-tail (cdr (member window all)))
         (me-rtail (cdr (member window (reverse all)))))
    (list me-rtail me-tail)))

(defun wa-resize-window (window size-diff edge)
  "Replacement code for `enlarge-window' and friends.
WINDOW is the Emacs window to resize.
SIZE-DIFF is the size difference.
EDGE is 'after or 'before which means left respectively right
edge for width etc.

First try to enlarge windows below WINDOW since that is probably
most visually appealing.  If that fails, then also try windows
above.

If that also fails, then ... - just fail for now.  This can be
expanded so that this strategy is applied upwards in the tree,
requesting parent window to be enlarged by the amount we need to
enlarge successfully. If that also fails (applying that all the
way up to the root window) then we must fail.

\(I do not have time to implement that right now, but I hope the
idea is clear. I think there are no problems with this since we
are just going upwards in the tree to the root.)

However there will still be a choice: failing with no enlargement
at all or failing with partial enlargement.

Return value?"
  ;; Fix-me: vertical
  (let* ((parent (wa-parent window))
         (new-size (+ size-diff (wa-wset window)))
         ret
         (siblings (wa-siblings window)) ;; includes WINDOW
         (sib-above (nth 0 siblings))
         (sib-below (nth 1 siblings))
         )
    (if (catch 'done
          ;; Set fixed computational size.
          (dolist (w sib-above) (wa-set-wfixed w (wa-wset w)))
          (dolist (w sib-below) (wa-set-wfixed w (wa-wset w)))
          (cond
           ((eq edge 'after) ;; After.
            (dolist (w sib-below)
              (wa-set-wfixed w nil)
              (unless (wa-compute-required-to-message parent)
                (throw 'done t))))
           ((eq edge 'before) ;; Before.
            (dolist (w sib-above)
              (wa-set-wfixed w nil)
              (unless (wa-compute-required-to-message parent)
                (throw 'done t))))
           (t (error "Bad arg, edge=%s" edge))))
        (progn
          (wa-compute-resulting win 'eq-sizes)
          (setq ret t))
      ;; Fix-me: Partial resizing etc?
      ;; Go up in tree if no fit. Fix-me: is this really needed? Is it good at all?
      (setq ret (wa-enlarge-window parent size)))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Accessing the real windows

(defun wa-get-frame-windows (frame horflag)
  "Get frame FRAME windows as a WA window tree.
If HORFLAG get the tree for horizontal dividing, otherwise for
vertical.

WA trees are those create with `wa-make-window',
`wa-set-child-windows' and friends.

Note 1: One WA can only be used for either horizontal or vertical
dividing.

Note 2: The routines for resizing Emacs windows from elisp works
for either horizontal or vertical dividing.  So WA and those
routines fits together."
  (setq frame (or frame (selected-frame)))
  (let ((window-tree (window-tree frame))
        (root-size (if horflag
                       (window-height (frame-root-window frame))
                     (window-width (frame-root-window frame))))
        (wa-tree (wa-make-window "Root" nil root-size nil nil))i
        )
    ))

(defun wa-get-tree-windows (win-subtree horflag)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing part

(defvar wa-root-window nil)

(defun wa-add-test-childs ()
  (wa-set-child-windows wa-root-window t
                         '(nil 12)
                         '(14 nil)
                         '(nil nil)
                         '(3 nil)
                         )
  (wa-set-child-windows (car (wa-child wa-root-window)) t
                        '(nil nil)
                        '(8 15))
  )

;; (wa-child wa-root-window)
;; (wa-wset wa-root-window)
;; (wa-wumin wa-root-window)
;; (wa-wumax wa-root-window)
;; (wa-clear-computed wa-root-window)

;; Setup
(setq wa-root-window (wa-make-window "Root" nil 80 nil nil  nil))
(setq wa-root-window (wa-make-window "Root" nil 80 nil 8  nil))
(setq wa-root-window (wa-make-window "Root" nil 80 nil 6  nil))
(setq wa-root-window (wa-make-window "Root" nil 43 15 nil))
(setq wa-root-window (wa-make-window "Root" nil 18 15 nil))
(setq wa-root-window (wa-make-window "Root" nil 15 15 nil))
(setq wa-root-window (wa-make-window "Root" nil 80 5 nil))

(wa-add-test-childs)
(wa-init-fail-flag     wa-root-window)
(setq wa-failed nil)

;; Show state now in case we want to stop on errors
(describe-variable    'wa-root-window)

;; Compute required, may fail.
(let ((msg (catch 'wa-fit-error
             (wa-set-wset wa-root-window (wa-wucur wa-root-window))
             (wa-compute-required wa-root-window)
             ;; Now it should not fail
             (wa-compute-resulting  wa-root-window 'eq-sizes))))
  (when msg
    (message "%s" (propertize msg 'face 'secondary-selection))))

;; Show final state
(describe-variable    'wa-root-window)
(with-current-buffer (help-buffer)
  (hi-lock-face-buffer "\"FAILED.*\"" 'hi-red-b)
  (hi-lock-face-buffer "OK" 'hi-green)
  (hi-lock-face-buffer "INIT" 'hi-blue))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; win-alg.el ends here
