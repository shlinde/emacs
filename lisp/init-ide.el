;;; init-ide.el --- IDE like features -*- lexical-binding: t; -*-
;;
;; Author: Sebastian Hempel Linde <boris@d12frosted.io>

;;; Commentary:
;;
;; Emacs can be an IDE. Sort of, thanks to `flycheck', `lsp-mode' and
;; `company'.
;;
;;; Code:

(require 'config-path)

(use-package vterm
  :ensure t
  :bind (("C-c t t" . vterm))
  :config
  (setopt vterm-kill-buffer-on-exit t
          vterm-max-scrollback 5000))

(use-package imenu
  :ensure nil
  :config
  (setopt imenu-auto-rescan t
          org-imenu-depth 3))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (delete 'c treesit-auto-langs)
  (delete 'cpp treesit-auto-langs)
  (global-treesit-auto-mode))

(setq-default treesit-font-lock-level 4)

(use-package company
  :ensure t
  :defer 2
  :diminish
  :defines (company-backends)
  :commands (company-complete-common
             company-manual-begin
             company-grab-line
             global-company-mode)
  :init
  (global-company-mode)
  (setq-default
   company-minimum-prefix-length 2
   company-tooltip-limit 14
   company-tooltip-align-annotations t
   company-require-match 'never
   company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode)
   company-backends '(company-capf)
   company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend)))

(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode)
  :defines (prescient-save-file)
  :commands (prescient-persist-mode)
  :config
  (setq prescient-save-file (concat path-cache-dir
                                    "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package flycheck
  :ensure t
  :defer 1
  :commands (global-flycheck-mode)
  :init
  (setq-default
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-check-syntax-automatically
   '(save idle-change mode-enabled)
   flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode +1))

;; See lisp/dash-functional.el for more information. Here we simply
;; make sure that our mock is loaded instead of upstream.
(eval-when-compile
  (let ((dir (expand-file-name "lisp" path-emacs-dir)))
    (delete dir load-path)
    (add-to-list 'load-path dir)))

(use-package lsp-mode
  :ensure t
  :hook
  (lsp-mode . lsp-lens-mode)
  :init
  (setq-default
   lsp-session-file (concat path-etc-dir "lsp-session")
   lsp-auto-guess-root nil
   lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

(use-package posframe
  :ensure t)

;; (use-package dap-mode
;;   :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode))

(use-package eglot
  :ensure t
  :defer t
  :defines (eglot-server-programs))

(use-package consult-lsp
  :ensure t
  :defer t
  :after lsp
  :config
  (define-key
    lsp-mode-map
    [remap xref-find-apropos]
    #'consult-lsp-symbols))

(provide 'init-ide)
;;; init-ide.el ends here
