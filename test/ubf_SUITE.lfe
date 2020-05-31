(defmodule ubf_SUITE
	(export all))

(include-lib "aelisp/include/aelisp.macro")

(func: all () '(mod_exists))

(func: mod_exists (_) (->
	`#(module ,M) = (~ code:load_file 'ubf)
	M))
