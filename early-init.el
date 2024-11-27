;;; early-init.el --- Early customization -*- lexical-binding: t; -*-
;;
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;
;; See Emacs Help for more information on The Early Init File.
;; Basically, this file contains frame customizations.
;;
;;; Code:

;; Enable using PLISTS
(setenv "LSP_USE_PLISTS" "true")

;; the sooner the better
(setq package-enable-at-startup nil)

;; Less clutter
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
;; (add-to-list 'default-frame-alist '(internal-border-width . 18))
(add-to-list 'default-frame-alist '(child-frame-border-width . 2))


(provide 'early-init)
;;; early-init.el ends here
