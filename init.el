;;; init.el --- Custom configurations -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;; Add `lisp' directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adjust garbage collection during startup
(setq gc-cons-threeshold (* 128 1024 1024))

;; Setup user
(setq-default user-full-name "Sebastian Hempel Linde"
              user-mail-address "sebastian@hempellinde.com")

;; Load the newest version of files
(setq-default load-prefer-newer t)

;; Bootstrap
(require 'config-path)
(require 'init-elpa)

;; Setup `custom-file'.
(setq custom-file (concat path-local-dir "custom.el"))

;; Load autoloads file
(unless elpa-bootstrap-p
  (unless (file-exists-p path-autoloads-file)
    (error "Autoloads file doesn't exist, please run '%s'"
           "eru install emacs"))
  (load path-autoloads-file nil 'nomessage))

;; Core
(require 'init-ui)

(provide 'init)
;;; init.el ends here
