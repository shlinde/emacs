;;; init-file-templates.el --- File templates support -*- lexical-binding: t; -*-
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module configures yasnippet.
;;
;;; Code:

(require 'config-path)
(require 'lib-fun)

(defvar file-templates-dir
  (expand-file-name "templates/" path-emacs-dir)
  "The path to yasnippet folders.")

(defvar file-templates-default-trigger "__"
  "The default yasnippet trigger key (a string).

Used for file template rules that don't have a :trigger property
in `file-templates-alist'.")

(defvar file-templates-alist
  `(
    ;; elisp
    ("dir-locals.el$"
     :ignore t)
    ("settings.el$"
     :ignore t)
    (emacs-lisp-mode
     :trigger "__package")
    (snippet-mode))
  "An alist of file template rules.

The CAR of each rule is either a major mode symbol or regexp
string.

The CDR is a plist. See `file-templates-set' for more
information.")

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-reload-all
             yas-dropdown-prompt
             yas--all-templates
             yas--get-snippet-tables
             yas--template-key)
  :hook ((text-mode . yas-minor-mode-on)
         (prog-mode . yas-minor-mode-on)
         (conf-mode . yas-minor-mode-on)
         (snippet-mode . yas-minor-mode-on))
  :init
  (advice-add #'yas-reload-all :around #'fun-silent)
  (setq-default yas-snippet-dirs '(file-templates-dir))
  (add-hook 'find-file-hook #'file-templates-check)
  :config
  (setq yas-prompt-functions (delq #'yas-dropdown-prompt
                                   yas-prompt-functions))
  ;; Ensure file templates in `file-templates-dir' are visible
  (yas-reload-all))

(defun file-template-insert-by-name (name)
  "Insert template by NAME."
  (cl-letf (((symbol-function 'dummy-prompt)
             (lambda (_prompt choices &optional display-fn)
               (or (cl-find name choices :key display-fn :test #'string=)
                   (throw 'notfound nil)))))
    (dlet ((yas-prompt-functions '(dummy-prompt)))
      (catch 'notfound
        (yas-insert-snippet t)))))

(defun file-templates-check ()
  "Check the current buffer for file template expansion.

Returns non-nil when current buffer is a candidate for file
template expansion.

The buffer must be non-read-only, empty, and there must be a rule
in `file-templates-alist' that applies to it."
  (when (and (not buffer-read-only)
             (bobp) (eobp)
             (not (string-match-p "^ *\\*" (buffer-name))))
    (let ((rule (cl-find-if #'file-templates-p
                            file-templates-alist)))
      (when rule (apply #'file-templates--expand rule)))))

(defun file-templates--set (pred plist)
  "Register a file template.

Refer to `file-templates-set' documentation on the meaning of
PRED and PLIST."
  (if (null (car-safe plist))
      (setq file-templates-alist
            (delq (assoc pred file-templates-alist)
                  file-templates-alist))
    (push `(,pred ,@plist) file-templates-alist)))

(defun file-templates-set (pred &rest plist)
  "Register a file template.

PRED can either be a regexp string or a major mode symbol.

PLIST may contain these properties:

  :when FUNCTION
    Provides a secondary predicate. This function takes no
    arguments and is executed from within the target buffer. If
    it returns nil, this rule will be skipped over.

  :trigger STRING|FUNCTION
    If a string, this is the yasnippet trigger keyword used to
    trigger the target snippet.

    If a function, this function will be run in the context of
    the buffer to insert a file template into. It is given no
    arguments and must insert text into the current buffer
    manually.

    If omitted, `file-templates-default-trigger' is used.

  :mode SYMBOL
    What mode to get the yasnippet snippet from. If omitted,
    either PRED (if it's a major-mode symbol) or the mode of the
    buffer is used.

  :project BOOL
    If non-nil, ignore this template if this buffer isn't in a
    project.

  :ignore BOOL
    If non-nil, don't expand any template for this file and don't
    test any other file template rule against this buffer.

\(fn PRED &key WHEN TRIGGER MODE PROJECT IGNORE)"
  (declare (indent defun))
  (file-templates--set pred plist))

(cl-defun file-templates--expand
    (pred &key project mode trigger ignore _when)
  "Auto insert a yasnippet snippet into current file.

See `file-templates-set' for information about PRED,
PROJECT, MODE, TRIGGER, IGNORE and _WHEN arguments."
  (when (and pred (not ignore))
    (when (if project (project-p) t)
      (unless mode
        (setq mode (if (symbolp pred) pred major-mode)))
      (unless mode
        (user-error "Couldn't determine mode for %s file template"
                    pred))
      (unless trigger
        (setq trigger file-templates-default-trigger))
      (if (functionp trigger)
          (funcall trigger)
        (yas-minor-mode-on)
        (yas-expand-snippet
         (yas-lookup-snippet trigger mode))))))

(defun file-templates-p (rule)
  "Return non-nil if the RULE apply to the current buffer."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (cond
          ((and (stringp pred)
                buffer-file-name)
           (string-match-p pred buffer-file-name))
          ((symbolp pred)
           (eq major-mode pred)))
         (or (not (plist-member plist :when))
             (funcall (plist-get plist :when)
                      buffer-file-name))
         rule)))

(defun file-templates-debug ()
  "Output the file template rule for current buffer.

Test the current buffer and outputs the file template rule most
appropriate for it. This is used for testing."
  (interactive)
  (message
   "Found %s"
   (cl-find-if
    #'file-templates-p
    file-templates-alist)))

(provide 'init-file-templates)
;;; init-file-templates.el ends here
