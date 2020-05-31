(defmodule s
	(export-macro s)
	(export all))

(include-lib "../include/aelp.macro")
(include-lib "../include/constant.macro")
(include-lib "../include/s.macro")

(macro: s ()
	"dynamically (without recompile this module) load all the macros in shell"
	`(progn
		(process_flag 'trap_exit 'true)
		(slurp ,(: file_lib find_source (: code which (MODULE))))))
