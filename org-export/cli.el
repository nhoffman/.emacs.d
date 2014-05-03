(setq lexical-binding t)
(provide 'cli)
(require 'package)

(defun cli-do-nothing () t)

(defun cli-option-p (str)
  "Return t if str is not nil and starts with '--'"
  (and str (string-equal (substring str 0 2) "--")))

(defun cli-opt-name (str)
  "Return the option name (strips leading '--')"
  (substring str 2 nil))

(defun cli-required-p (optdef)
  "Return t if the option defined in `optdef` (an element of
`options-alist') is required."
  (eq (length optdef) 2)
  )

(defun cli-parse-args (options-alist &optional arguments)
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
value of `cli-do-nothing'.
"

  (setq command-line-functions '(cli-do-nothing))
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
		  (puthash (cli-opt-name (car optdef)) (nth 2 optdef) args))
	      ) options-alist)

    ;; set options from command line arguments
    (while clargs
      (setq opt (car clargs))
      (setq optname (if (cli-option-p opt) (cli-opt-name opt)))
      (if optname
	  (progn
	    (setq optiondef (assoc opt options-alist))
	    (if optiondef
		;; If val is provided, add it to args. Otherwise,
		;; store a value of t unless the option is required.
		(progn
		  (unless (cli-option-p (nth 1 clargs)) (setq val (nth 1 clargs)))
		  (if (not (cli-required-p optiondef)) (setq val (or val t)))
		  (puthash optname val args))
	      (error (format "Error: the option '%s' is not defined." opt)))
	    ))
      (setq clargs (cdr clargs)))

    ;; check for required arguments
    (mapc #'(lambda (optdef)
	      (setq hashval (gethash (cli-opt-name (car optdef)) args))
	      (if (and (eq (length optdef) 2) (not hashval))
		  (error (format "Error: a value for the option '%s' is required."
				 (car optdef))))
	      ) options-alist)

    args
    ))

;; package management

(defun cli-package-installed-not-builtin-p (package &optional min-version)
  "Return true if PACKAGE, of MIN-VERSION or newer, is installed,
ignoring built in packages.  MIN-VERSION should be a version
list."
  (let ((pkg-desc (assq package package-alist)))
    (if pkg-desc
        (version-list-<= min-version
                         (package-desc-vers (cdr pkg-desc))))))

(defun cli-install-packages (package-list)
  "Install each package named in PACKAGE-LIST using elpa if not
already installed."
  (mapc #'(lambda (pkg)
	    (unless (cli-package-installed-not-builtin-p pkg)
	      (package-menu-refresh)
	      (package-install pkg))
	    ) package-list))

(defvar cli-default-package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ))

(defun cli-package-setup (emacs-directory package-list &optional upgrade archives)
  (setq user-emacs-directory emacs-directory)
  (package-initialize)
  (setq package-archives (or archives cli-default-package-archives))
  (package-list-packages-no-fetch)
  (if upgrade
      (progn
	(package-menu-mark-upgrades)
	(package-menu-execute)))
  (cli-install-packages package-list))

