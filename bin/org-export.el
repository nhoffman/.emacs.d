(load-file "bin/common.el")

(require-option "org-file" args)
(require-option "export-type" args) ;; html, el
(require-option "output-file" args)

;; save the current directory; find-file seems to change it
(setq cwd default-directory)

;; copy org-mode file to a temp file
(setq body-temp (make-temp-name "temp-body-"))
(copy-file (gethash "post" args) body-temp t)
(find-file body-temp)
(org-mode)

;; compile and save body only
;; (setq org-export-with-toc nil)
;; (org-export-as-html 3 ;; levels of TOC
;; 		    nil ;; EXT-PLIST
;; 		    "string" ;; TO-BUFFER
;; 		    t ;; BODY-ONLY
;; 		    )

;; (write-file (gethash "html-body" args))

;; (setq default-directory cwd)
;; (delete-file body-temp)
