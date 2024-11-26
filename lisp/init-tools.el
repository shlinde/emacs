;;; init-tools.el --- various tools configurations -*- lexical-binding: t; -*-
;;
;; Author: Boris Buliga <boris@d12frosted.io>

;;; Commentary:
;;
;;; Code:

(use-package restclient
  :ensure (:host github :repo "pashky/restclient.el" :files ("restclient.el" "restclient-jq.el"))
  :defer t
  :config
  (require 'restclient-jq))

(use-package jq-mode
  :ensure t
  :defer t)

(use-package gptel
  :ensure t
  :defer t
  :config
  (setq gptel-model "gpt-4o"
        gptel-directives
        (let ((file (expand-file-name "gptel-directives.el" vulpea-directory)))
          (when (file-exists-p file)
            (with-temp-buffer
              (condition-case nil
	          (progn
	            (insert-file-contents file)
                    (read (current-buffer)))
	        (error
	         (message "Could not read data from %s" file))))))))

(provide 'init-tools)
;;; init-tools.el ends here
