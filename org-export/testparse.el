(require 'argparse (concat (file-name-directory load-file-name) "argparse.el"))

;; get command line options
;; (setq infile (get-option args "infile"))
;; (setq outfile (get-option args "outfile"))

;; (print args)

(setq options-alist
      '(("infile" "foo")
	("--outfile" "bar")
	("--buh"))
      )

(print options-alist)
(print (cdr (assoc "--outfile" options-alist)))
(print (assoc "--buh" options-alist))
