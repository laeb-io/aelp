(defmodule atm
	(export all))

(include-lib "../include/aelisp.macro")

(func: + (A B) (lta! (++ (atl A) (atl B))))
