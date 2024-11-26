;;; config-path.el --- Path constants -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(defconst path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.")

(defconst path-data-dir (concat path-home-dir "data/")
  "Path to user data directory.")

(defconst path-emacs-dir  (file-name-as-directory user-emacs-directory)
  "Path to Emacs configuration.")

(defconst path-autoloads-file
  (expand-file-name "lisp/init-autoloads.el" path-emacs-dir)
  "Path to personal autoloads file.")

(defconst path-local-dir
  (file-name-as-directory (concat path-home-dir ".cache"))
  "Path to cache directory")

(defconst path-etc-dir (concat path-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data")

(defconst path-cache-dir (concat path-local-dir "cache/")
  "Directory for volatile storage.")

(defconst path-packages-dir
  (expand-file-name (format "packages/%s.%s"
                            emacs-major-version
                            emacs-minor-version)
                    path-local-dir)
  "Path to directory containing packages")

(defconst path-projects-dir
  (file-name-as-directory
   (or (getenv "PROJECTS_HOME")
       (concat path-data-dir "source/")))
  "Root directory for projects.")

(defconst path-memex-directory
  (file-name-as-directory
   (or (getenv "MEMEX_HOME")
       (concat path-data-dir "memex/")))
  "Root directory for notes.")

(provide 'config-path)
;;; config-path.el ends here
