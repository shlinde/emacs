;;; lib-vulpea-id.el --- description -*- lexical-binding: t; -*-
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

(require 'org-id)

(defvar vulpea-id-auto-targets nil
  "Targets for automatic ID assignment.

Each element of this list can be one of the following:

- file - to automatically set ID on the file level;
- headings - to automatically set ID for each heading in the file.

Empty list means no id assignment is needed.")

;;;###autoload
(defun vulpea-id-auto-assign ()
  "Add ID property to the current file.

Targets are defined by `vulpea-id-auto-targets'."
  (when (and vulpea-id-auto-targets
             (derived-mode-p 'org-mode)
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (seq-contains-p vulpea-id-auto-targets 'file)
        (org-id-get-create))
      (when (seq-contains-p vulpea-id-auto-targets 'headings)
        (org-map-entries #'org-id-get-create)))))

(provide 'lib-vulpea-id)
;;; lib-vulpea-id.el ends here
