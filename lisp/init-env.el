;;; init-env.el --- Environment configurations -*- lexical-binding: t; -*-
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;; Code:

(defconst env-graphic-p (display-graphic-p))
(defconst env-rootp (string-equal "root" (getenv "USER")))
(defconst env-sys-linux-p (eq system-type 'gnu/linux))
(defconst env-sys-wsl-p (when (string-match "-[Mm]icrosoft" operating-system-release) t))
(defconst env-sys-name (system-name))

(use-package exec-path-from-shell
  :ensure t
  :hook (after-init . exec-path-from-shell-initialize))


(provide 'init-env)
;;; init-env.el ends here
