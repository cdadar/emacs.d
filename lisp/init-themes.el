;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; macOS publishes its appearance in the global `AppleInterfaceStyle' default:
;; set to "Dark" in dark mode, absent otherwise.  Reading it with `defaults'
;; needs nothing from the system beyond a subprocess.
;;
;; This is deliberately not done with AppleScript (`auto-dark' does that,
;; through `ns-do-applescript' + System Events): in an Emacs launched by
;; Finder/Dock that call intermittently fails with "AppleScript error 1"
;; -- Emacs's status for "no script result" -- and since the error is
;; raised from `auto-dark-mode' itself, the mode never gets as far as
;; installing its timer, so the appearance is not followed at all.
(defun cdadar/macos-dark-mode-p ()
  "Return non-nil when macOS is in dark mode."
  (string= "Dark"
           (string-trim
            (shell-command-to-string
             "defaults read -g AppleInterfaceStyle 2>/dev/null"))))

(defun cdadar/preferred-theme ()
  "Return the Modus theme matching the current system appearance."
  (if (and *is-a-mac* (cdadar/macos-dark-mode-p)) 'modus-vivendi 'modus-operandi))

;; Keep the macOS titlebar and scrollbars in sync with the active theme.
(defun cdadar/refresh-ns-appearance (&rest _)
  "Match the frame's `ns-appearance' parameter to its `background-mode'."
  (when (featurep 'ns)
    (let ((bg (frame-parameter nil 'background-mode)))
      (set-frame-parameter nil 'ns-appearance bg)
      (setf (alist-get 'ns-appearance default-frame-alist) bg))))

(defun cdadar/follow-macos-appearance (&rest _)
  "Load the Modus theme that matches the current system appearance."
  (let* ((dark (and *is-a-mac* (cdadar/macos-dark-mode-p)))
         (theme (cdadar/preferred-theme)))
    (unless (memq theme custom-enabled-themes)
      (mapc #'disable-theme (copy-sequence custom-enabled-themes))
      (load-theme theme t))
    ;; Loading a theme does not update the frame's `background-mode', which
    ;; `cdadar/refresh-ns-appearance' reads; `frame-background-mode' is what
    ;; tells Emacs which of the two the frame is now in.
    (setq frame-background-mode (if dark 'dark 'light))
    (mapc #'frame-set-background-mode (frame-list))
    (cdadar/refresh-ns-appearance)))

;; Theme configuration — modus-themes is built into Emacs and loaded via
;; `load-theme', but does not provide a `modus-themes' feature that can be
;; required.  We use `:no-require t' to prevent `use-package' from calling
;; `require', while `:demand t' still ensures the :custom/:config/:bind are
;; evaluated eagerly at startup.
(use-package modus-themes
  :ensure nil
  :no-require t
  :demand t
  :custom
  ;; Add all your customizations prior to loading the themes
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-region '(bg-only no-extend))
  :config
  ;; Start with the theme the system is set to, so a dark-mode session does
  ;; not flash light.  `<f5>' still toggles by hand.
  (cdadar/follow-macos-appearance)
  :bind
  (("<f5>" . modus-themes-toggle)))

(add-hook 'enable-theme-functions #'cdadar/refresh-ns-appearance)

(when *is-a-mac*
  ;; `focus-in-hook' is the cheap trigger: it fires when the user comes back
  ;; to Emacs after switching the appearance.  The timer covers a switch made
  ;; while Emacs stays focused.
  (add-hook 'focus-in-hook #'cdadar/follow-macos-appearance)
  (run-with-timer 60 60 #'cdadar/follow-macos-appearance))



(defun sanityinc/dimmer-refresh-after-background-mode-change (&rest _)
  "Refresh dimmer after frame background mode changes."
  (dimmer-process-all))

(use-package dimmer
  :hook
  ((after-init . dimmer-mode))
  :custom
  (dimmer-fraction 0.15)
  :config
  ;; TODO: file upstream as a PR
  (advice-add 'frame-set-background-mode :after #'sanityinc/dimmer-refresh-after-background-mode-change)

  ;; Don't dim in terminal windows. Even with 256 colours it can
  ;; lead to poor contrast.  Better would be to vary dimmer-fraction
  ;; according to frame type.
  (defun sanityinc/display-non-graphic-p ()
    (not (display-graphic-p)))
  (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p))

(use-package minions
  :bind
  (([S-down-mouse-3] . minions-minor-modes-menu))
  :hook
  ((after-init . minions-mode)))

(provide 'init-themes)
;;; init-themes.el ends here
