;;; init-ui.el --- UI configurations -*- lexical-binding: t; -*-
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;; Code:

(require 'init-env)

;; no startup  screen
(setq inhibit-startup-screen t)

;; no startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; initial buffer
(setq initial-buffer-choice nil)

;; no frame title
(setq frame-title-format nil)

;; no file dialog
(setq use-file-dialog nil)

;; no dialog box
(setq use-dialog-box nil)

;; no empty line indicators
(setq indicate-empty-lines nil)

;; no cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; (setq initial-scratch-message nil)
(setq inhibit-default-init t)

;; start easy with little dependencies to load
(setq initial-major-mode 'fundamental-mode)

;; yet keep `text-mode' as default major mode
(setq default-major-mode 'text-mode)

;; maximum font lock
(setq font-lock-maximum-decoration t)

;; no confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; no beeping and no blinking please
(setq ring-bell-function #'ignore)
(setq visible-bell nil)

;; make sure that trash is not drawn
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)

(use-package minions
  :ensure t
  :init
  (minions-mode 1))

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; don't resize emacs in steps, it looks weird and plays bad with
;; window manager.
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; disable bidirectional text for tiny performance boost
(setq bidi-display-reordering nil)

;; no clutter, please!
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; when theme is right, this thing is good
(use-package hl-line
  :ensure nil
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)))

;;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (display-time-mode)
  (display-battery-mode)
  (setq doom-modeline-battery t
        doom-modeline-time t
        doom-modeline-env-version t))

;;; Compilation
;;(use-package fancy-compilation
;;  :ensure (:host codeberg :repo "ideasman42/emacs-fancy-compilation")
;;  :commands (fancy-compilation-mode)
;;  :init
;;  (setf fancy-compilation-override-colors nil)
;;  (with-eval-after-load 'compile
;;    (fancy-compilation-mode)))

;;; Theme
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  (modus-themes-select 'modus-vivendi))

;;; Fonts
(use-package fontaine
  :ensure t
  :if env-graphic-p
  :config
  (setq fontaine-presets
        '((regular
           :default-height 120
           :default-family "ZedMono Nerd Font")
          (medium
           :default-weight semilight
           :default-height 140)
          (large
           :default-weight semilight
           :default-height 180
           :bold-weight extrabold)
          (t ; our shared fallback properties
           :default-family "Source Code Pro"
           :default-weight normal
           ;; :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "Source Code Pro"
           :variable-pitch-weight normal
           :variable-pitch-height 1.05
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
