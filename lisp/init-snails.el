(require 'snails)

(global-set-key (kbd "M-P") (lambda ()
                              (interactive)
                              (snails '(snails-backend-projectile
                                        snails-backend-buffer
                                        snails-backend-imenu
                                        snails-backend-recentf))))

(provide 'init-snails)
