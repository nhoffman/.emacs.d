(provide 'argparse)

;; functions for processing command line arguments
(defun is-option (str)
  "Return true if string looks like a command line option"
  (string-equal (substring str 0 1) "-"))

;; TODO - make optional without requiring a default
(defun get-option (args opt &optional default)
  "Return the value of 'opt' from 'args'; if there is no value for
  'opt' return 'default' if provided, otherwise raise an error."
  (or (or (gethash opt args) default)
      (error (format "Error: option -%s is required" opt))))

;; allows arbitrary command line arguments
(defun do-nothing () t)
(setq command-line-functions '(do-nothing))

;; store option, value pairs in hash-map `args`
;; http://ergoemacs.org/emacs/elisp_hash_table.html
(defvar args (make-hash-table :test 'equal))

;; process command-line-args
(setq clargs command-line-args)
(while clargs
  (setq opt (car clargs))
  (setq val (car (cdr clargs)))
  (if (and (is-option opt) (not (is-option val)))
      (puthash (substring opt 1 nil) val args))
  (setq clargs (cdr clargs)))

