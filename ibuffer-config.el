;; switch between groups using ibuffer-switch-to-saved-filter-groups
(setq ibuffer-saved-filter-groups
      (quote (
	      
	      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	      ;; (mostly) organized by directory
	      ("default"

	       ;;;; separately versioned projects
	       ;; notes
	       ("notes"
	      	(filename . "Dropbox/notes/"))
	       
	       ;; qdashboard
	       ("qdash-config"
	      	(filename . "/qdashboard/config"))

	       ("qdash-data-TAT"
	      	(filename . "/qdashboard/data/TAT"))
	       ("qdash-data-cancelCredit"
	      	(filename . "/qdashboard/data/cancelCredit"))
	       ("qdash-data"
	      	(filename . "/qdashboard/data"))

	       ("qdash-metrics-TAT"
	      	(filename . "/qdashboard/metrics/TAT"))
	       ("qdash-metrics-collectionBatches"
	      	(filename . "/qdashboard/metrics/collectionBatches"))
	       ("qdash-metrics"
	      	(filename . "/qdashboard/metrics"))

	       ("qdash-reports"
	      	(filename . "/qdashboard/reports"))

	       ("qdash-R-code"
	      	(filename . "/qdashboard/qdashboard.R/R"))
	       ("qdash-R-man"
	      	(filename . "/qdashboard/qdashboard.R/man"))
	       ("qdash-R-package"
	      	(filename . "/qdashboard/qdashboard.R"))

	       ("qdashboard"
	      	(filename . "/qdashboard/"))

	       ;; sconstools
	       ("sconstools"
	      	(filename . "/src/sconstools"))

	       ;; bioscons
	       ("bioscons-package"
	      	(filename . "/bioscons/bioscons"))
	       ("bioscons-tests"
	      	(filename . "/bioscons/tests"))
	       ("bioscons"
	      	(filename . "/bioscons"))
	       
	       ;; Seq
	       ("Seq-tests"
	      	(filename . "Seq/tests"))
	       ("Seq-taxonomy"
	      	(filename . "Seq/taxonomy"))
	       ("Seq-scripts"
	      	(filename . "Seq/scripts"))	       
	       ("Seq"
	      	(filename . "Seq/"))

	       ;; taxtastic
	       ("taxtastic-scripts"
		(filename . "/taxtastic/taxtastic/scripts/"))
	       ("taxtastic-subcommands"
		(filename . "/taxtastic/taxtastic/subcommands/"))
	       ("taxtastic-package"
		(filename . "/taxtastic/taxtastic"))
	       ("taxtastic-tests"
		(filename . "/taxtastic/tests"))
	       ("taxtastic"
		(filename . "/taxtastic/"))

	       ("clinlabr"
	      	(filename . "/clinlabr/"))
	   
	       ;; zsh
	       ("zsh"
	      	(filename . "zsh/"))

	       ("blast2tree/tests"
	      	(filename . "blast2tree/tests"))
	       ("blast2tree-package"
	      	(filename . "blast2tree/blast2tree"))
	       ("blast2tree"
	      	(filename . "blast2tree"))
	       ("labqc"
	      	(filename . "labqc/"))
	       
	       ;; moinlm 
	       ("moinlm-config"
	      	(filename . "moinlm/config"))
	       ("moinlm-plugin-action"
	      	(filename . "moinlm/plugin/action"))
	       ("moinlm-plugin-macro"
	      	(filename . "moinlm/plugin/macro"))
	       ("moinlm-plugin-theme"
	      	(filename . "moinlm/plugin/theme"))
	       ("moinlm"
	      	(filename . "moinlm/"))

	       ;; rounduplm
	       ("tracker-it"
	      	(filename . "src/rounduplm/tracker/it"))
	       ("tracker-sps"
	      	(filename . "src/rounduplm/tracker/sps"))
	       ("tracker-micro"
	      	(filename . "src/rounduplm/tracker/micro"))
	       ("tracker-common"
	      	(filename . "src/rounduplm/tracker/common"))
	       ("rounduplm"
	      	(filename . "src/rounduplm"))

	       ;; people
	       ("people-tests"
	      	(filename . "src/people/tests"))
	       ("people"
	      	(filename . "src/people"))

	       ("clst_R"
	      	(filename . "/clst/R/"))
	       ("clst_tests"
	      	(filename . "/clst/tests/"))
	       ("clst_man"
	      	(filename . "/clst/man/"))
	       ("clst"
	      	(filename . "/clst/"))

	       ("clstutils_R"
	      	(filename . "/clstutils/R/"))
	       ("clstutils_man"
	      	(filename . "/clstutils/man/"))
	       ("clstutils_tests"
	      	(filename . "/clstutils/tests/"))
	       ("clstutils"
	      	(filename . "/clstutils/"))

	       ("classify16s_tests"
	      	(filename . "/classify16s/tests/"))
	       ("classify16s"
	      	(filename . "/classify16s/"))

	       ;; projects under bvdiversity
	       ("bv-classify"
	      	(filename . "bvdiversity/scripts/classify"))
	       ("bv-align"
	      	(filename . "bvdiversity/scripts/align"))
	       ("bv-pplacer"
	      	(filename . "bvdiversity/scripts/pplacer"))
	       ("bv-ref"
	      	(filename . "bvdiversity/scripts/ref"))
	       ("bv-metadata"
	      	(filename . "bvdiversity/scripts/metadata"))
	       ("bv-reports"
	      	(filename . "bvdiversity/scripts/reports"))
	       ("bv-scripts"
	      	(filename . "bvdiversity/scripts/"))

	       ;; reference sets
	       ("refset-gut"
	      	(filename . "/refset/gut/"))
	       ("refset-stomach"
	      	(filename . "/refset/stomach/"))
	       ("refset-vaginal"
	      	(filename . "/refset/vaginal/"))
	       ("refset-blast"
	      	(filename . "/refset/blast/"))
	       ("refset-rdpseqs"
	      	(filename . "/refset/rdpseqs/"))
	       ("refset"
	      	(filename . "/refset/"))

	       ;; pipelines
	       ("vaginal-analysis-p5-tiny"
	      	(filename . "/vaginal_analysis/plate5_tiny"))
	       ("vaginal-analysis-p5-nonoise"
	      	(filename . "/vaginal_analysis/plate5_nonoise"))
	       ("vaginal-analysis-p5"
	      	(filename . "/vaginal_analysis/plate5"))
	       ("vaginal-analysis-p1to4-trimmed"
	      	(filename . "/vaginal_analysis/plates1to4_trimmed"))
	       ("vaginal-analysis-p1to4"
	      	(filename . "/vaginal_analysis/plates1to4"))
	       ("vaginal-analysis-bin"
	      	(filename . "/vaginal_analysis/bin"))
	       ("vaginal-analysis"
	      	(filename . "/vaginal_analysis/"))

	       ("vaginal-pipeline-nonoise"
	      	(filename . "/vaginal/vaginal-nonoise"))
	       ("vaginal-pipeline-plates1to4"
	      	(filename . "/vaginal/vaginal-plates1to4"))
	       ("vaginal-pipeline"
	      	(filename . "/vaginal/vaginal-master"))


	       ("stomach-pipeline"
	      	(filename . "/stomach/stomach-master"))

	       ;; classcompare
	       ("classcompare/pipeline"
	      	(filename . "/classcompare/pipeline/"))
	       ("classcompare"
	      	(filename . "/classcompare/"))

	       ;; working/nhoffman
	       ("clst" (filename . "working/nhoffman/clst"))
	       ("clst-old" (filename . "working/nhoffman/clst-old"))
	       ("report_template" (filename . "working/nhoffman/report_template"))
	       ("grantupdate" (filename . "working/nhoffman/grantupdate"))
	       ("20100817_poster" (filename . "working/nhoffman/20100817_poster"))
	       ("20100902_lacobacillus_seqs" (filename . "working/nhoffman/20100902_lacobacillus_seqs"))
	       ("20100908_sujatha_aligns" (filename . "working/nhoffman/20100908_sujatha_aligns"))
	       ("20100914_taxtable" (filename . "working/nhoffman/20100914_taxtable"))
	       ("20100915_refseqs" (filename . "working/nhoffman/20100915_refseqs"))
	       ("20100916_amplicon" (filename . "working/nhoffman/20100916_amplicon"))
	       ("reclassify" (filename . "working/nhoffman/reclassify"))
	       ("20100920_leaveoneout" (filename . "working/nhoffman/20100920_leaveoneout"))
	       ("20100928_Dialister_type3" (filename . "working/nhoffman/20100928_Dialister_type3"))
	       ("20101002_sujatha_aligns" (filename . "working/nhoffman/20101002_sujatha_aligns"))
	       ("20101020_scons_parallelization_example" (filename . "working/nhoffman/20101020_scons_parallelization_example"))
	       ("20101021_sujatha_queries" (filename . "working/nhoffman/20101021_sujatha_queries"))
	       ("20101104_hmp_seqs" (filename . "working/nhoffman/20101104_hmp_seqs"))
	       ("20101022_placedb_example" (filename . "working/nhoffman/20101022_placedb_example"))
	       ("20101105_leaveoneout" (filename . "working/nhoffman/20101105_leaveoneout"))
	       ("20101116_firmicutes" (filename . "working/nhoffman/20101116_firmicutes"))
	       ("20101208_bacteroidetes" (filename . "working/nhoffman/20101208_bacteroidetes"))
	       ("20101207_no_match" (filename . "working/nhoffman/20101207_no_match"))
	       ("20101208_lactobacillaceae" (filename . "working/nhoffman/20101208_lactobacillaceae"))
	       ("20101213_bacteroidetes_chlorobi_group" (filename . "working/nhoffman/20101213_bacteroidetes_chlorobi_group"))
	       ("20101214_sujatha_aligns" (filename . "working/nhoffman/20101214_sujatha_aligns"))
	       ("20101227_aligns" (filename . "working/nhoffman/20101227_aligns"))
	       ("20110204_test_lactos" (filename . "working/nhoffman/20110204_test_lactos"))
	       ("20110505_plate5_aligns" (filename . "working/nhoffman/20110505_plate5_aligns"))
	       ("working/nhoffman" (filename . "working/nhoffman"))
	       
	       ("working" (filename . "/working/"))

	       ;; catch anything else in bvdiversity
	       ("bvdiversity"
	      	(filename . "bvdiversity/"))
	       
	       ;; pplacer
	       ("pplacer"
	      	(filename . "src/pplacer/"))

	       ;; projects under seqtax
	       ("place-feb12"
	      	(filename . "seqtax/bv/place-12-Feb-2010/"))
	       ("place-mar11"
	      	(filename . "seqtax/bv/place-11-Mar-2010/"))
	       ("place-devel"
	      	(filename . "seqtax/bv/place-devel/"))
	       ("place-longitudinal"
	      	(filename . "seqtax/bv/place/"))
	       ("make-refs"
	      	(filename . "seqtax/bv/make_refs/"))
	       ("rdp"
	      	(filename . "seqtax/bv/rdp_dataset/"))
	       ("ncbi-taxonomy"
	      	(filename . "seqtax/bv/ncbi_taxonomy/"))
	       ("bv"
	      	(filename . "seqtax/bv/"))

	       ("mdx-localseqs"
	      	(filename . "seqtax/mdx/localseqs/"))
	       ("mdx-rdp"
	      	(filename . "seqtax/mdx/rdp/"))
	       ("mdx"
	      	(filename . "seqtax/mdx/"))
	       ("mbari-scripts"
	      	(filename . "seqtax/mbari/scripts"))
	       ("mbari"
	      	(filename . "seqtax/mbari/"))
	       ("pyro"
	      	(filename . "seqtax/pyro/"))

	       ;; teaching
	       ("lm322-2010-stats"
	      	(filename . "/teaching/20101008_lm322_three_lectures/lm322-2010-10-08-stats/"))
	       ("lm322-2010-qm"
	      	(filename . "/teaching/20101008_lm322_three_lectures/lm322-2010-10-11-qm/"))
	       ("lm322-2010-lis"
	      	(filename . "/teaching/20101008_lm322_three_lectures/lm322-2010-10-13-lis/"))
	       ("teaching"
	      	(filename . "/teaching/"))

	       ;; labmed projects 
	       ("deltas"
	      	(filename . "/deltas/"))
	       ("papers"
	      	(filename . "/papers/"))
	  
	       ;; stuff in Dropbox
	       ("dropbox-career"
	      	(filename . "Dropbox/career"))
	       ("vancouver-poster"
	      	(filename . "Dropbox/presentations/20110309_vancouver"))
	       ("dropbox-presentations"
	      	(filename . "Dropbox/presentations"))
	       ("dropbox-scratch"
	      	(filename . "Dropbox/scratch"))
	       ("dropbox"
	      	(filename . "Dropbox/"))

	       ;; various presentations
	       ("090225_lm510_16sClassification"
	      	(filename . "presentations/090225_lm510_16sClassification"))
	       ("201008_tenMinutesOfResearch"
	      	(filename . "presentations/2010_tenMinutesOfResearch"))
	       
	       ;; projects under p_acnes
	       ("p_acnes"
	      	(filename . "p_acnes/"))

	       ;; catch remaining files by mode
	       ("R-files"
	      	(or
	      	 (mode . ess-mode)
	      	 (mode . R-mode)
	      	 ))

	       ("py-files"
	      	(mode . python-mode))

	       ("emacs-lisp"
	      	(mode . emacs-lisp-mode))	       
	       )
	      
	      ;; ;;;;;;;;;;;;;;;;;
	      ;; organize entirely by mode
	      ("mode"
	       ("zsh"
		(filename . "zsh/"))
	       ("R-files"
		(or
		 (mode . ess-mode)
		 (mode . R-mode)
		 ))
	       ("py-files"
		(mode . python-mode))
	       ("emacs-lisp"
		(mode . emacs-lisp-mode))
	       )
	      
	      )
	     ))
