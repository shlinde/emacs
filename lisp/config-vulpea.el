;;; config-vulpea.el --- description -*- lexical-binding: t; -*-
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

(require 'config-path)

(defvar vulpea-test-mode
  (file-exists-p
   (expand-file-name "vulpea_test" path-cache-dir))
  "Non-nil if notes should start in a test mode.

Probably that means using directory with test notes instead of
real notes. Maybe it also means experimental features.")

(defvar vulpea-directory
  (expand-file-name
       "memex/"
       path-data-dir)
  "Directory containing notes.")

(provide 'config-vulpea)
;;; config-vulpea.el ends here
