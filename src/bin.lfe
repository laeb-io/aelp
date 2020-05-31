(defmodule bin
	(export all))

(include-lib "../include/aelisp.macro")

(func: first (B N)
	(~ binary:part B 0 N))

(func: last (B N) (->
	Len = (?bin B)
	(~ binary:part B (- Len N) N)))	

(func: prefix (P B) '())

(func: suffix (S B) '())

(func: reverse_to_list (B) (reverse_to_list B '()))

(func: reverse_to_list
	(#"" Acc) Acc
	(B Acc) (->
		(~> (X (sz 8) integer) (Y bs)) = B
		(reverse_to_list Y `(,X . ,Acc))))

(func: reverse_bin (B) (ltb (reverse_to_list B)))

(func: match_right (B P) (->*
	B1 = (reverse_bin B)
	P1 = (reverse_bin P)
	`#(,RI ,Len) = (~ binary:match B1 P1)
	PLen = (?bin P)
	`#(,(- (?bin B) RI PLen) ,PLen)))
	
(func: split_right (B P) (->*
	`#(,X ,Y) = (match_right B P)
	(~> (B1 (sz X) bs) (_B2 (sz Y) bs) (B3 bs)) = B
	`#(,B1 ,B3)))
