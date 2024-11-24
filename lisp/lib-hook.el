;;; lib-hook.el --- Hook utilities -*- lexical-binding: t; -*-
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;
;; Taming hooks using some utilities.
;;
;;; Code:

(require 'lib-fun)

(defmacro hook-with-delay (hook secs function &optional depth local)
  "Add the FUNCTION to the value of HOOK.

The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.

DEPTH and LOCAL are passed as is to `add-hook'."
  (let* ((f-name-str (concat (symbol-name (fun-unquote function))
                             "-"
                             (symbol-name (fun-unquote hook))
                             "-with-delay"))
         (f-name (make-symbol f-name-str))
         (doc (format "Call `%s' in %s seconds"
                      (symbol-name (fun-unquote function))
                      secs)))
    `(progn
       (eval-when-compile
         (defun ,f-name () ,doc
                (run-with-idle-timer ,secs nil ,function))
         (add-hook ,hook #',f-name ,depth ,local)))))

(provide 'lib-hook)
;;; lib-hook.el ends here
