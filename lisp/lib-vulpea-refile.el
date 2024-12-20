;;; lib-vulpea-refile.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024, Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;; Maintainer: Sebastian Hempel Linde <sebastian@hempellinde.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 24 Nov 2024
;;
;; URL: https://github.com/d12frosted/
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'org)

(defvar vulpea-refile-ignored-tags '("JOURNAL" "REFILE")
  "List of tags to ignore during refile.")

;;;###autoload
(defun vulpea-refile-verify-target ()
  "Exclude todo keywords with a done state from refile targets."
  (let ((tags-at (org-get-tags)))
    (and
     ;; doesn't have done keyword
     (not (member (nth 2 (org-heading-components)) org-done-keywords))

     ;; doesn't have blacklisted tag
     (or (null tags-at)
         (cl-member-if-not
          (lambda (x)
            (member (if (listp x) (car x) x)
                    vulpea-refile-ignored-tags))
          tags-at)))))

(provide 'lib-vulpea-refile)
;;; lib-vulpea-refile.el ends here
