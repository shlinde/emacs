;;; init-selection.el --- Selection configurations -*- lexical-binding: t; -*-
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;
;; Selection system configurations. Choose from ivy, selectrum and
;; consult.
;;
;;; Code:

(require 'init-elpa)
(require 'config-path)



(use-package vertico
  :ensure (vertico :files ("*.el" "extensions/*.el"))
  :init
  (vertico-mode))

(use-package vertico-repeat
  :ensure nil
  :bind ("M-R" . vertico-repeat)
  :hook ((minibuffer-setup . vertico-repeat-save)))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package emacs
  :ensure nil
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(defvar selection-orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

(defun selection-orderless--suffix-regexp ()
  "."
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
              consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

(defun selection-orderless-dispatch (word _index _total)
  "Custom style dispatcher based to use with `orderless' completion.

Based on `selection-orderless-dispatch-alist'.

See `orderless-style-dispatchers' to learn about WORD, _INDEX and
_TOTAL arguments."
  (cond
   ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1) (selection-orderless--suffix-regexp))))
   ;; File extensions
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
    `(orderless-regexp . ,(concat "\\." (substring word 1) (selection-orderless--suffix-regexp))))
   ;; Ignore single !
   ((equal "!" word) `(orderless-literal . ""))
   ;; Prefix and suffix
   ((if-let (x (assq (aref word 0) selection-orderless-dispatch-alist))
        (cons (cdr x) (substring word 1))
      (when-let (x (assq (aref word (1- (length word))) selection-orderless-dispatch-alist))
        (cons (cdr x) (substring word 0 -1)))))))

(use-package orderless
  :ensure t
  :functions (orderless-define-completion-style)
  :config
  ;; Define orderless style with initialism by default
  (orderless-define-completion-style selection-orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles partial-completion))
          (command (styles selection-orderless-with-initialism))
          (variable (styles selection-orderless-with-initialism))
          (symbol (styles selection-orderless-with-initialism)))
        ;; allow escaping space with backslash!
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers '(selection-orderless-dispatch)))

(use-package marginalia
  :ensure (:wait t)
  :demand t
  :commands (marginalia-mode
             marginalia-cycle)
  :init
  (marginalia-mode)
  ;; When using Selectrum, ensure that Selectrum is refreshed when
  ;; cycling annotations.
  (advice-add #'marginalia-cycle
              :after
              (lambda ()
                (when (bound-and-true-p selectrum-mode)
                  (selectrum-exhibit))))

  (setq-default marginalia-annotators
                '(marginalia-annotators-heavy
                  marginalia-annotators-light
                  nil)))

(use-package consult
  :ensure t
  :bind
  (("M-y" . consult-yank-pop))
  :general
  (leader-def
    "bb" '(consult-buffer :which-key "Switch buffer")
    "pg" '(consult-grep :which-key "Grep the project")
    "ji" '(consult-imenu :which-key "imenu")))

(use-package embark
  :ensure t
  :bind
  (("M-." . embark-dwim)
   ("C-." . embark-act)))

(use-package embark-conuslt
  :defer t)

(use-package transient
  :ensure t
  :defer t
  :init
  (setq
   transient-levels-file (expand-file-name
                          "transient/levels.el"
                          path-cache-dir)
   transient-values-file (expand-file-name
                          "transient/values.el"
                          path-cache-dir)
   transient-history-file (expand-file-name
                           "transient/history.el"
                           path-cache-dir)))

(provide 'init-selection)
;;; init-selection.el ends here
