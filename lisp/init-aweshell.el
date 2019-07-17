;;; init-aweshell.el --- awesome support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(require 'aweshell)

(global-set-key (kbd "<f12>") #'aweshell-dedicated-toggle)

(provide 'init-aweshell)
