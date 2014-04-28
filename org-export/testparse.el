(require 'argparse (concat (file-name-directory load-file-name) "argparse.el"))

;; get command line options
(setq infile (get-option args "infile"))
(setq outfile (get-option args "outfile"))

(print args)
