;;; init-python.el --- Python configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024, Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;; Maintainer: Sebastian Hempel Linde <sebastian@hempellinde.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 26 Nov 2024
;;
;; URL: https://github.com/d12frosted/
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;; Virtual Environments
(defun shl-python-hook ()
  "Activate virtual environment and start LSP."
  (let* ((venv_path (concat (locate-dominating-file "." "pyproject.toml") ".venv/")))
    (with-eval-after-load 'pyvenv
      (pyvenv-activate venv_path))
    (with-eval-after-load 'lsp-mode
      (require 'lsp-pyright)
      (lsp))))

(use-package python
  :config
  (setopt python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode))

(use-package pyvenv
  :ensure t
  :demand t
  :hook (python-base-mode . shl-python-hook))


(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright")) ;; or basedpyright

(provide 'init-python)
;;; init-python.el ends here
