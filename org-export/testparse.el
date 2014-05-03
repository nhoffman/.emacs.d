(require 'cli (concat (file-name-directory load-file-name) "cli.el"))

(byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))

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

;; (print (describe-function 'cli-parse-args))

(setq args (cli-parse-args options-alist))
(print args)
;; (print (cli-parse-args options-alist '("--infile" "FOO" "--outfile" "BAR")))



