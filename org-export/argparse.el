(provide 'argparse)

;; functions for processing command line arguments
(defun is-option (str)
  "Return t if str is not nil and starts with '--'"
  (string-equal (substring str 0 2) "--"))

;; TODO - make optional without requiring a default
(defun get-option (args opt &optional default)
  "Return the value of 'opt' from 'args'; if there is no value for
  'opt' return 'default' if provided, otherwise raise an error."
  (or (or (gethash opt args) default)
      (error (format "Error: option -%s is required" opt))))

(print command-line-args)

(defun argparse-do-nothing () t)

(defun argparse-parse-args (options-alist)
  ;; allow arbitrary command line arguments by defining
  ;; `command-line-functions` globally (Warning: side effect!)
  (setq command-line-functions '(argparse-do-nothing))
  (let ((clargs command-line-args)
	(args (make-hash-table :test 'equal))
	(opt nil)
	(val nil)
	(argdef nil))
    (while clargs
      (setq opt (car clargs))
      (setq val (car (cdr clargs)))
      (if (is-option opt)
	  (progn
	    (setq argdef (or (assoc opt options-alist)
		(error (format "Error: option %s is not defined" opt))))
	    (puthash
	     (substring opt 2 nil) (if (or (eq val nil) (is-option val)) t val) args)))
      (setq clargs (cdr clargs)))
    args
    ))

;; (setq argmap (argparse-parse-args))

;; store option, value pairs in hash-map `args`
;; http://ergoemacs.org/emacs/elisp_hash_table.html
;; (defvar args (make-hash-table :test 'equal))

;; process command-line-args
;; (setq clargs command-line-args)
;; (while clargs
;;   (setq opt (car clargs))
;;   (setq val (car (cdr clargs)))
;;   (if (and (is-option opt) (not (is-option val)))
;;       (puthash (substring opt 1 nil) val args))
;;   (setq clargs (cdr clargs)))

