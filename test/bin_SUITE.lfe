(defmodule bin_SUITE
	(export all))

(include-lib "aelp/include/aelp.macro")

(func: all () '(first last reverse_to_list split_right))

(func: first (_) (->
	#"ab" = (~ bin:first #"abcd" 2)))

(func: last (_) (->
	#"cd" = (~ bin:first #"cd" 2)))

(func: reverse_to_list (_) (->
	X = (list #\d #\c #\b #\a)
	Y = (~ bin:reverse_to_list #"abcd")
	(=== X Y)))

(func: split_right (_) (->
	#(#"a/b" #"/c.lfe") = (~ bin:split_right #"a/b/b/c.lfe" #"/b")))
