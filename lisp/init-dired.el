;;; init-dired.el --- Dired configurations -*- lexical-binding: t; -*-

;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;
;; Configurations for wonderful `dired'.
;;
;;; Code:

(use-package dired
  :ensure nil
  :init
  (setq
   dired-listing-switches "-alh"
   dired-recursive-copies 'always
   dired-recursive-deletes 'top
   dired-auto-revert-buffer t
   dired-hide-details-hide-symlink-targets nil))

;; sort dired buffer so directories are first
(add-hook 'dired-after-readin-hook #'dired-sort-directories-first)

(defun dired-sort-directories-first ()
  "List directories first in Dired buffers."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (set-buffer-modified-p nil))

(provide 'init-dired)
;;; init-dired.el ends here
