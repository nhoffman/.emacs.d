;;; ob-pygment.el --- org-babel functions for pygment evaluation

;; Copyright (C) Jianing Yang

;; Author: Jianing Yang
;; Keywords: Syntax Highlighting
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Examples
;;
;; (require 'ob-pygment)
;;
;; ; --- path to pygmentize
;; (setq org-pygment-path "/usr/bin/pygmentize")
;;
;; (setq org-babel-load-languages (quote (pygment . t)))
;;


;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:pygment
  '((:results . "html") (:exports . "results"))
  "Default arguments to use when evaluating a pygment source block.")

(defun org-babel-execute:pygment (body params)
  "Execute a block of Dot code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
     (out-file (cdr (assoc :file params)))
     (cmdline (cdr (assoc :cmdline params)))
     (in-file (org-babel-temp-file "pygment-"))
     (cmd (concat org-pygment-path
             " " cmdline
             " " (org-babel-process-file-name in-file)
  )))
    (unless (file-exists-p org-pygment-path)
      (error "Could not find pygment at %s" org-pygment-path))
    (message (concat "Running Pygment: " cmd))
    (with-temp-file in-file (insert body))
    (org-babel-eval cmd "")
  ))

(defun org-babel-prep-session:pygment (session params)
  "Return an error because Dot does not support sessions."
  (error "Dot does not support sessions"))

(provide 'ob-pygment)

;;; ob-pygment.el ends here
