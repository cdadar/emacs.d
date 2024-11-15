;;; init-epub.el --- epub   -*- lexical-binding: t -*-

;; Copyright (C) 2024 chens
;;
;; Version: 0.0.1
;; Keywords: epub
;; Author: chens <chens AT linux-bszb>
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;;; Code:

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(provide 'init-epub)
;;; init-epub.el ends here
