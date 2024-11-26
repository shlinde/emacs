;;; init-elpa.el --- Initialize ELPA -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(require 'config-path)

(defvar elpa-bootstrap-p nil)

(setq package-user-dir
      (expand-file-name "elpa/" path-packages-dir))

;; Bootstrap Elpaca
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" path-packages-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(when elpa-bootstrap-p
  (elpaca-generate-autoloads "init" (expand-file-name "lisp/" path-emacs-dir)))

(when elpa-bootstrap-p
  (defun elpa--log-wrapper (fn &rest args)
    "Add messages buffer logging to `elpaca--log'.

FN is called with ARGS."
    (let ((e (nth 0 args))
          (txt (nth 1 args)))
      (message "[%s] %s" (elpaca<-id e) txt))
    (apply fn args))
  (advice-add 'elpaca--log :around #'elpa--log-wrapper))

;; Install `use-package' support

(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode))

(setq-default use-package-enable-imenu-support t)

;; Critical packages
(use-package s :ensure (:wait t) :demand t)
(use-package dash :ensure (:wait t) :demand t)
(use-package gcmh
  :ensure t
  :hook (emacs-startup . (lambda ()
                           (gcmh-mode 1))))

;; 'common' packages
(use-package async :ensure t :defer t)
(use-package ts :ensure t :defer t)
(use-package request
  :ensure t
  :defer t
  :init
  (setq-default
   request-storage-directory (expand-file-name "request" path-cache-dir)))

(use-package request-deferred
  :ensure t
  :defer t)

(use-package esup
  :ensure t
  :defer t
  :init
  ;; https://github.com/progfolio/elpaca/issues/23
  (setq esup-depth 0))

(provide 'init-elpa)
;;; init-elpa.el ends here
