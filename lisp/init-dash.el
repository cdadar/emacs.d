;; Support for the http://kapeli.com/dash documentation browser

(defun sanityinc/dash-installed-p ()
  "Return t if Dash is installed on this machine, or nil otherwise."
  (let ((lsregister "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister"))
    (and (file-executable-p lsregister)
         (not (string-equal
               ""
               (shell-command-to-string
                (concat lsregister " -dump|grep com.kapeli.dash")))))))

(when (and *is-a-mac* (not (package-installed-p 'dash-at-point)))
  (message "Checking whether Dash is installed")
  (when (sanityinc/dash-installed-p)
    (require-package 'dash-at-point)))

(when (package-installed-p 'dash-at-point)
  (global-set-key (kbd "C-c D") 'dash-at-point))


(defun cdadar/zeal-installed-p ()
  (let ((zeal "/usr/bin/zeal"))
    (file-executable-p zeal)))

(when (and *linux* (not (package-installed-p 'zeal-at-point)))
  (message "CHecking whether Zeal in installed")
  (when (cdadar/zeal-installed-p)
    (require-package 'zeal-at-point)))

(when (package-installed-p 'zeal-at-point)
  (global-set-key (kbd "C-c D") 'zeal-at-point))


(provide 'init-dash)
