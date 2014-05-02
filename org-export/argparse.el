(setq lexical-binding t)
(provide 'argparse)

(defun argparse-do-nothing () t)

(defun argparse-option-p (str)
  "Return t if str is not nil and starts with '--'"
  (and str (string-equal (substring str 0 2) "--")))

(defun argparse-get-name (str)
  "Return the option name (strips leading '--')"
  (substring str 2 nil))

(defun argparse-required-p (optdef)
  "Return t if the option defined in `optdef` (an element of
`options-alist') is required."
  (eq (length optdef) 2)
  )

(defun argparse-parse-args (options-alist &optional arguments)
  "Parses a list of arguments according to the specifiction
provided by `options-alist' and returns a hashmap of option names
to values. Arguments are read from `command-line-args' unless an
optional list `arguments' is provided.

`options-alist' is of the form

'((required-option help-text)
  (option help-text default-value)
  (boolean-option help_text nil))

Each option must be a string beginning with '--'. If
default-value is not provided, the option is required and an
error will be raised if it is missing.

Arguments are of the form '--option <value>'. If 'value' is
missing and a default value is defined, 'option' will be given a
value of t (this is useful for defining boolean command line
parameters).

Note that this function has a side effect: arbitrary command line
arguments are allowed by assigning `command-line-functions` a
value of `argparse-do-nothing'.
"

  (setq command-line-functions '(argparse-do-nothing))
  (let ((clargs (or arguments command-line-args))
	(args (make-hash-table :test 'equal))
	(opt nil)
	(optname nil)
	(optiondef nil)
	(val nil)
	(hashval nil))

    ;; set defaults
    (mapc #'(lambda (optdef)
	      (if (eq (length optdef) 3)
		  (puthash (argparse-get-name (car optdef)) (nth 2 optdef) args))
	      ) options-alist)

    ;; set options from command line arguments
    (while clargs
      (setq opt (car clargs))
      (setq optname (if (argparse-option-p opt) (argparse-get-name opt)))
      (if optname
	  (progn
	    (setq optiondef (assoc opt options-alist))
	    (if optiondef
		;; If val is provided, add it to args. Otherwise,
		;; store a value of t unless the option is required.
		(progn
		  (unless (argparse-option-p (nth 1 clargs)) (setq val (nth 1 clargs)))
		  (if (not (argparse-required-p optiondef)) (setq val (or val t)))
		  (puthash optname val args))
	      (error (format "Error: the option '%s' is not defined." opt)))
	    ))
      (setq clargs (cdr clargs)))

    ;; check for required arguments
    (mapc #'(lambda (optdef)
	      (setq hashval (gethash (argparse-get-name (car optdef)) args))
	      (if (and (eq (length optdef) 2) (not hashval))
		  (error (format "Error: a value for the option '%s' is required."
				 (car optdef))))
	      ) options-alist)

    args
    ))
