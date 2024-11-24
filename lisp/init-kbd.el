;;; init-kbd.el --- Keybinding configurations -*- lexical-binding: t; -*-
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;; Code:

(require 'init-env)

(defmacro leader-def (&rest args)
  "A wrapper for `general-def'."

  (declare (indent defun))
  `(,'general-def ,@args ,@'(:states nil
                             :keymaps 'override
                             :prefix "M-m"
                             :prefix-command 'prefix-command
                             :prefix-map 'prefix-map)))
	   
(use-package general
  :ensure (:wait t)
  :demand t
  :init
  (leader-def
    "/" '(nil :which-key "search...")
    "[" '(nil :which-key "previous...")
    "a" '(nil :which-key "align...")
    "g" '(nil :which-key "git...")
    "i" '(nil :which-key "insert...")
    "j" '(nil :which-key "jump...")
    "o" '(nil :which-key "open...")
    "v" '(nil :which-key "vino...")))

(use-package which-key
  :ensure (:wait t)
  :demand t
  :diminish which-key-mode
  :init
  (which-key-mode))

(defvar kbd-escape-hook nil
  "A hook run after \\[keyboard-quit] is pressed.

Triggers `kbd-escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun kbd-escape ()
  "Run the `kbd-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop
        ;; there.
        ((cl-find-if #'funcall kbd-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'kbd-escape)

(provide 'init-kbd)
;;; init-kbd.el ends here
