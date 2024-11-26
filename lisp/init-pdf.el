;;; init-pdf.el --- PDF viewing support -*- lexical-binding: t; -*-
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;
;; PDF viewing support.
;;
;; Building pdf-tools on macOS can be daunting. One of the solutions
;; is to use nix package manager (from nix user perspective ,nix is a
;; solution for every problem):
;;
;; $ cd $PACKAGES/straight/build/pdf-tools/build/server
;; $ nix-shell -p pkg-config poppler autoconf automake libtool libpng
;; $ autoreconf -i -f
;; $ ./autobuild -i $PACKAGES/straight/build/pdf-tools --os nixos
;;
;; See https://github.com/politza/pdf-tools/issues/645 for more
;; information.
;;
;;; Code:

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . auto-revert-mode)
  :commands (pdf-info-close
             pdf-tools-install)
  :init
  (setq-default pdf-view-display-size 'fit-page)
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-setup-view-mode))

(defun pdf-setup-view-mode ()
  "Setup `pdf-view-mode'."
  (add-hook 'kill-buffer-hook #'pdf-cleanup-windows nil t)
  (cua-mode 0))

(defun pdf-cleanup-windows ()
  "Kill left-over annotation buffers on document death."
  (when (and (boundp 'pdf-annot-list-document-buffer)
             (buffer-live-p pdf-annot-list-document-buffer))
    (pdf-info-close pdf-annot-list-document-buffer))
  (when (and (boundp 'pdf-annot-list-buffer)
             (buffer-live-p pdf-annot-list-buffer))
    (kill-buffer pdf-annot-list-buffer))
  (let ((contents-buffer (get-buffer "*Contents*")))
    (when (and contents-buffer (buffer-live-p contents-buffer))
      (kill-buffer contents-buffer))))

(provide 'init-pdf)
;;; init-pdf.el ends here
