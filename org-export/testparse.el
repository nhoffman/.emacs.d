(require 'argparse (concat (file-name-directory load-file-name) "argparse.el"))

;; get command line options
;; (setq infile (get-option args "infile"))
;; (setq outfile (get-option args "outfile"))

;; (print args)

(setq options-alist
      '(("--infile" "input file")
	("--outfile" "output file")
	("--foo" "an argument named foo" "bar"))
      )

;; (print options-alist)
;; (print (cdr (assoc "--outfile" options-alist)))
;; (print (assoc "--buh" options-alist))

(print (argparse-parse-args options-alist))


