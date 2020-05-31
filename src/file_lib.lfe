(defmodule file_lib
	(export all))

(include-lib "../include/aelp.macro")

(func: find_source (Path) (->*
	FileName = (~ filename:basename Path)
	DirName = (~ filename:dirname Path)
	`#(ok ,SourcePath) = (~ filelib:find_source FileName DirName
		'(#(".beam" ".lfe" (#("ebin" "src")))))
	SourcePath))
