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

(use-package company
  :ensure t
  :defer 2
  :diminish
  :defines (company-backends)
  :commands (company-complete-common
             company-manual-begin
             company-grab-line
             global-company-mode)
  :bind (:map company-active-map
              ("C-y" . company-complete-selection)
              ("RET" . nil))
  :init
  (global-company-mode)
  (setopt company-minimum-prefix-length 1
          company-idle-delay 0.0
          company-abort-on-unique-match nil
          company-search-regexp-function 'company-search-flex-regexp
          company-tooltip-align-annotations t
          company-global-modes
          '(not erc-mode message-mode help-mode gud-mode eshell-mode)
          company-backends '(company-files
                             company-capf)
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
  (setq-default flycheck-emacs-lisp-load-path 'inherit
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
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-lens-mode)
  :init
  (setq-default lsp-session-file (concat path-etc-dir "lsp-session")
                lsp-auto-guess-root nil
                lsp-keep-workspace-alive nil)

  (setopt lsp-clients-clangd-args '("--background-index=false" "--header-insertion-decorators" "--log=error")
          lsp-enable-file-watchers t
          lsp-idle-delay 0.25
          read-process-output-max (* 1024 1024)
          lsp-modeline-diagnostics-enable nil
          lsp-completion-provider :none
          lsp-imenu-index-function #'lsp-imenu-create-categorized-index)

  (setopt lsp-enable-indentation nil
          lsp-enable-on-type-formatting nil))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

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
