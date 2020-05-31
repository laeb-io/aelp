(defmodule atm
	(export all))

(include-lib "../include/aelp.macro")

(func: + (A B) (lta! (++ (atl A) (atl B))))
