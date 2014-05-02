(setq lexical-binding t)
(provide 'argparse)

;; TODO - make optional without requiring a default
(defun get-option (args opt &optional default)
  "Return the value of 'opt' from 'args'; if there is no value for
  'opt' return 'default' if provided, otherwise raise an error."
  (or (or (gethash opt args) default)
      (error (format "Error: option -%s is required" opt))))

(print command-line-args)

(defun argparse-do-nothing () t)

(defun argparse-option-p (str)
  "Return t if str is not nil and starts with '--'"
  (string-equal (substring str 0 2) "--"))

(defun argparse-get-name (str)
  "Return the option name (strips leading '--')"
  (substring str 2 nil))

(defun argparse-parse-args (options-alist)
  ;; allow arbitrary command line arguments by defining
  ;; `command-line-functions` globally (Warning: side effect!)
  (setq command-line-functions '(argparse-do-nothing))
  (let ((clargs command-line-args)
	(args (make-hash-table :test 'equal))
	(opt nil)
	(optname nil)
	(val nil))

    ;; set defaults
    (mapc #'(lambda (optdef)
	      (if (nth 2 optdef)
		  (puthash (argparse-get-name (car optdef)) (nth 2 optdef) args))
	      ) options-alist)

    ;; define options from command line arguments
    (while clargs
      (setq opt (car clargs))
      (setq optname (if (argparse-option-p opt) (argparse-get-name opt)))
      (setq val (car (cdr clargs)))
      (if optname
	  (progn
	    ;; unless instead of or?
	    (unless (assoc opt options-alist)
		(error (format "Error: option '%s' is not defined" opt)))
	    (puthash
	     optname (if (or (eq val nil) (argparse-option-p val)) t val) args)))
      (setq clargs (cdr clargs)))

    ;; check for required arguments
    (mapc #'(lambda (optdef)
	      (unless (gethash (argparse-get-name (car optdef)) args)
		(error (format "Error: option '%s' is required" (car optdef))))
	      ) options-alist)

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

