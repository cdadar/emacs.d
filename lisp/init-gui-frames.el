;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Stop C-z from minimizing windows under OS X

(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(use-package frame
  :ensure nil
  :bind (("C-z" . sanityinc/maybe-suspend-frame)
         ("C-M-8" . sanityinc/decrease-opacity)
         ("C-M-9" . sanityinc/increase-opacity)
         ("C-M-7" . sanityinc/reset-opacity))
  :custom
  (use-file-dialog nil)
  (use-dialog-box nil)
  (inhibit-startup-screen t)
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  (frame-title-format
   '((:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
              "%b"))))
  :config
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
  (menu-bar-mode -1)
  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border)))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(defun sanityinc/decrease-opacity ()
  "Decrease the current frame opacity slightly."
  (interactive)
  (sanityinc/adjust-opacity nil -2))

(defun sanityinc/increase-opacity ()
  "Increase the current frame opacity slightly."
  (interactive)
  (sanityinc/adjust-opacity nil 2))

(defun sanityinc/reset-opacity ()
  "Reset the current frame opacity to 100."
  (interactive)
  (modify-frame-parameters nil '((alpha . 100))))

(use-package ns-auto-titlebar
  :if *is-a-mac*
  :config
  (ns-auto-titlebar-mode))

(use-package ns-win
  :ensure nil
  :if *is-a-mac*
  :bind (("M-ƒ" . toggle-frame-fullscreen)))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(use-package term
  :ensure nil
  :hook (term-mode . (lambda ()
                       (setq line-spacing 0))))


(use-package disable-mouse)

;; Unbind mouse bindings for text-scale-mode
(dolist (bind '("C-<wheel-down>" "C-<wheel-up>" "C-<mouse-4>" "C-<mouse-5>"))
  (define-key global-map (kbd bind) nil))


(use-package pixel-scroll
  :ensure nil
  :config
  (pixel-scroll-precision-mode))


;;
;; Tile the frame: Rectangle-style halves, thirds and two-thirds
;;
;; Rectangle's own Control+Option+arrow shortcuts are not reliably delivered
;; to Emacs on macOS, and inside Emacs those keys are already
;; `backward-sexp'/`forward-sexp'/`backward-up-list'/`down-list', which are
;; worth more in this configuration.  So the tiling lives under `C-c v'
;; (which-key lists it): arrows for halves, letters for thirds, m/u for
;; maximize and restore.  `s-<left>' is `move-beginning-of-line' here.
;;
;; Move them onto `C-M-<arrow>' if you would rather have Rectangle's keys.

(defvar cdadar-frame-tiling-gap 0
  "Pixels to keep between a tiled frame and the edge of the workarea.
Set it to something like 8 to leave a visible margin around every frame.")

(defvar cdadar--frame-geometry-before-tiling nil
  "Outer rectangle saved by the tiling commands, put back by
`cdadar/frame-restore'.")

(defun cdadar--frame-outer-rect ()
  "Return the selected frame's outer rectangle as (LEFT TOP WIDTH HEIGHT).
`frame-edges' is used rather than `frame-pixel-width'/`frame-pixel-height':
on NS the latter report the width and the height on different footings (the
width includes the window decorations, the height excludes the titlebar)."
  (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges nil 'outer-edges)))
    (list left top (- right left) (- bottom top))))

(defun cdadar--tiling-save-geometry ()
  "Remember the selected frame's outer rectangle.
The `left'/`top'/`width'/`height' frame parameters are not used for this:
they are in characters on this build, and restoring them lands the frame
somewhere else."
  (setq cdadar--frame-geometry-before-tiling (cdadar--frame-outer-rect)))

(defun cdadar--tiling-fullscreen-p ()
  "Return non-nil when the selected frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun cdadar--tiling-apply (left top width height)
  "Put the selected frame's outer rectangle at LEFT TOP WIDTH HEIGHT.
On NS `set-frame-size' sizes the *window including its decorations*, so the
result comes out wider and taller than asked -- by a constant amount that is
not worth hardcoding (Centaur uses -20/-30; it follows the frame's font and
decorations).  Measure the overshoot and aim again instead: one correction
is enough in practice, the extra rounds only guard against it changing."
  (let ((req-w width)
        (req-h height)
        (done nil)
        (attempt 0))
    (while (and (not done) (< attempt 4))
      (set-frame-position nil left top)
      (set-frame-size nil req-w req-h t)
      (let* ((rect (cdadar--frame-outer-rect))
             (dw (- width (nth 2 rect)))
             (dh (- height (nth 3 rect))))
        (if (and (<= (abs dw) 1) (<= (abs dh) 1))
            (setq done t)
          (setq req-w (+ req-w dw)
                req-h (+ req-h dh)
                attempt (1+ attempt)))))))

(defun cdadar/tile-frame (left-fraction width-fraction top-fraction height-fraction)
  "Put the selected frame at a fraction of its monitor's workarea.
The four arguments are fractions of the workarea: LEFT-FRACTION and
TOP-FRACTION are the offset of the frame's left and top edge, the other two
its size.  Write them as floats -- (0 0.5 0 1) is the left half, and
(/ 1.0 3) a third (1/3 would be integer division and yield 0)."
  (unless (display-graphic-p)
    (user-error "Cannot tile a frame on a text terminal"))
  (if (cdadar--tiling-fullscreen-p)
      ;; Leaving native fullscreen is animated, so the new geometry only
      ;; sticks once it has finished.
      (progn
        (set-frame-parameter nil 'fullscreen nil)
        (run-at-time 0.4 nil #'cdadar/tile-frame
                     left-fraction width-fraction top-fraction height-fraction))
    (cdadar--tiling-save-geometry)
    ;; Clear a `maximized' fullscreen state, which would override the geometry.
    (set-frame-parameter nil 'fullscreen nil)
    (let* ((area (frame-monitor-workarea))
           (gap cdadar-frame-tiling-gap)
           (ax (nth 0 area))
           (ay (nth 1 area))
           (aw (nth 2 area))
           (ah (nth 3 area))
           (left (+ ax (round (* aw left-fraction)) gap))
           (top (+ ay (round (* ah top-fraction)) gap))
           (width (- (round (* aw width-fraction)) (* 2 gap)))
           (height (- (round (* ah height-fraction)) (* 2 gap))))
      (cdadar--tiling-apply left top width height))))

(defun cdadar/frame-maximize ()
  "Fill the frame's monitor workarea."
  (interactive)
  (cdadar/tile-frame 0 1 0 1))

(defun cdadar/frame-left-half ()
  "Put the frame in the left half of the workarea."
  (interactive)
  (cdadar/tile-frame 0 0.5 0 1))

(defun cdadar/frame-right-half ()
  "Put the frame in the right half of the workarea."
  (interactive)
  (cdadar/tile-frame 0.5 0.5 0 1))

(defun cdadar/frame-top-half ()
  "Put the frame in the top half of the workarea."
  (interactive)
  (cdadar/tile-frame 0 1 0 0.5))

(defun cdadar/frame-bottom-half ()
  "Put the frame in the bottom half of the workarea."
  (interactive)
  (cdadar/tile-frame 0 1 0.5 0.5))

(defun cdadar/frame-left-third ()
  "Put the frame in the left third of the workarea."
  (interactive)
  (cdadar/tile-frame 0 (/ 1.0 3) 0 1))

(defun cdadar/frame-center-third ()
  "Put the frame in the middle third of the workarea."
  (interactive)
  (cdadar/tile-frame (/ 1.0 3) (/ 1.0 3) 0 1))

(defun cdadar/frame-right-third ()
  "Put the frame in the right third of the workarea."
  (interactive)
  (cdadar/tile-frame (/ 2.0 3) (/ 1.0 3) 0 1))

(defun cdadar/frame-left-two-thirds ()
  "Make the frame fill the left two thirds of the workarea."
  (interactive)
  (cdadar/tile-frame 0 (/ 2.0 3) 0 1))

(defun cdadar/frame-right-two-thirds ()
  "Make the frame fill the right two thirds of the workarea."
  (interactive)
  (cdadar/tile-frame (/ 1.0 3) (/ 2.0 3) 0 1))

(defun cdadar/frame-restore ()
  "Put the frame back where it was before the last tiling command."
  (interactive)
  (if cdadar--frame-geometry-before-tiling
      (progn
        (set-frame-parameter nil 'fullscreen nil)
        (apply #'cdadar--tiling-apply cdadar--frame-geometry-before-tiling))
    (user-error "Nothing tiled in this session yet")))

(defvar cdadar-frame-tiling-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'cdadar/frame-left-half)
    (define-key map (kbd "<right>") #'cdadar/frame-right-half)
    (define-key map (kbd "<up>") #'cdadar/frame-top-half)
    (define-key map (kbd "<down>") #'cdadar/frame-bottom-half)
    (define-key map "l" #'cdadar/frame-left-third)
    (define-key map "c" #'cdadar/frame-center-third)
    (define-key map "r" #'cdadar/frame-right-third)
    (define-key map "L" #'cdadar/frame-left-two-thirds)
    (define-key map "R" #'cdadar/frame-right-two-thirds)
    (define-key map "m" #'cdadar/frame-maximize)
    (define-key map "u" #'cdadar/frame-restore)
    map)
  "\`C-c v' prefix for tiling the selected frame.")

(define-key global-map (kbd "C-c v") cdadar-frame-tiling-map)


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
