;;; init.el --- Custom configurations -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;; Add `lisp' directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adjust garbage collection during startup
(setq gc-cons-threshold (* 128 1024 1024))

;; Setup user
(setq-default user-full-name "Sebastian Hempel Linde"
              user-mail-address "sebastian@hempellinde.com")

;; Load the newest version of files
(setq-default load-prefer-newer t)

;; Bootstrap
(defvar bootstrap-p t
  "First run?")

(require 'config-path)
(require 'init-elpa)

(defun ensure-directory-exists (path)
  "Ensure that PATH exists as directory.
Create if not."
  (unless (file-directory-p path)
    (make-directory path)))

(defun setup-directories ()
  "Setup directories needed."
  (mapc #'ensure-directory-exists
        `(,path-home-dir
          ,path-data-dir
          ,path-emacs-dir
          ,path-local-dir
          ,path-etc-dir
          ,path-cache-dir
          ,path-packages-dir
          ,path-projects-dir
          ,path-memex-directory)))

(when bootstrap-p
  (setup-directories))

;; Setup `custom-file'.
(setq custom-file (concat path-local-dir "custom.el"))

;; Load autoloads file
(unless elpa-bootstrap-p
  (unless (file-exists-p path-autoloads-file)
    (error "Autoloads file doesn't exist, please run '%s'"
           "eru install emacs"))
  (load path-autoloads-file nil 'nomessage))


;; Core
(require 'init-env)
(require 'init-kbd)
(require 'init-editor)
(require 'init-ui)
(require 'init-buffer)
(require 'init-window)

;; Utilities
(require 'init-selection)
(require 'init-project)
(require 'init-vcs)
(require 'init-ide)
(require 'init-file-templates)
(require 'init-dired)
(require 'init-pdf)
(require 'init-tools)

;; Memex
(require 'init-memex)

;; Languages
(require 'init-elisp)
(require 'init-cc)

(provide 'init)
;;; init.el ends here
