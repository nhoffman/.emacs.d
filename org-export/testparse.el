(require 'argparse (concat (file-name-directory load-file-name) "argparse.el"))

(byte-compile-file (concat (file-name-directory load-file-name) "argparse.el"))

;; get command line options
;; (setq infile (get-option args "infile"))
;; (setq outfile (get-option args "outfile"))

;; (print args)

;; (--option help default)
(setq options-alist
      '(("--infile" "input file")
	("--outfile" "output file")
	("--foo" "an argument named foo" "bar")
	("--baz" "make baz true" nil)
	))

;; (print (describe-function 'argparse-parse-args))

(print (argparse-parse-args options-alist))
;; (print (argparse-parse-args options-alist '("--infile" "FOO" "--outfile" "BAR")))



