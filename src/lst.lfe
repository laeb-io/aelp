(defmodule lst
	(export all))

(include-lib "../include/aelp.macro")

(func: reverse_to_binary
	'() #""
	`(,H . ,T) (~> (H (sz 8) integer) ((reverse_to_binary T) bs)))

(func: last (N L)
	(~ lists:sublist L (- (?lst L) (-1, N)) N))

(func: first (N L)
    (~ lists:sublist L N))

(func: delete (N L) (delete N 1 L))

(func: delete
    (1 1 `(,_ . ,T)) T
    (1 Len `(,_ . ,T)) (delete 1 (-1, Len) T)
    (N Len `(,H . ,T)) `(,H . ,(delete (-1, N) Len T)))

(func: insert
    (0 L X) `(,X . ,L)
    (1 `(,H . ,T) X) `(,H ,X . ,T)
    (N `(,H . ,T) X) `(,H . ,(insert (-1, N) T X)))

(func: set (N L X) (set N L X 1))
(func: set
    (N `(,_ . ,T) X N) `(,X . ,T)
    (N `(,H . ,T) X I)
        `(,H . ,(set N T X (+1, I))))

;Todo: (lst:set_all N L '(#(1 a) #(5 b)))
;(func: set_all (N L T) (set N L T 1))
;(func: set_all
;    (N `(,_ . ,T) X I) (&= N I) `(,X . ,T)
;    (N `(,H . ,T) X I)
;        `(,H . ,(set N T X (+1. I))))

(func: flat
    '() '()
    `(() . ,T) 
        (flat T)
    `((,H1 . ,T1) . ,T)
        `(,H1 . ,(flat `(,T1 . ,T)))
    `(,H . ,T)
        `(,H . ,(flat T)))

(func: unchain (L) (unchain L 2))

(func: unchain (L N) (->*
	F\2 = (\
		(X `#(,M ,Part ,Acc)) (&= M N)
			`#(1 () ((,X . ,Part) . ,Acc))
		(X `#(,M ,Part ,Acc))
			`#(,(+1, M) (,X . ,Part) ,Acc))
	`#(,_ ,_ ,Acc) = (reduce F\2 #(1 () ()) L)
	Acc))

(func: unchain (L N K) (unchain L N K (\_. 'false) #(0 ())))

(func: unchain (L N K S) (unchain L N K S #(0 ())))

(func: unchain
	(() N _ S `#(,_ ,Part)) (->
		Part' = (reverse Part) (:=
		(< (length Part') N) `#(() ,Part')
		(=:= (length Part') N) (:- (:. S Part')
			`#(() ,Part')
			`#((,Part') ()))
		'true
			`#((,Part') ())))

	((= `(,H . ,T) L) N K S `#(,C ,Part)) (&= C N) (:- (:. K H)
		(unchain T N K S `#(,C ,`(,H . ,Part))) (->
		Part2 = (reverse Part) (:- (:. S Part2)
		`#(() ,(++ Part2 L)) (->
		`#(,X ,Y) = (unchain L N K S #(0 ()))
		`#((,Part2 . ,X) ,Y)))))

	(`(,H . ,T) N K S `#(,C ,Part)) (&< C N) (:- (:. K H)
		(unchain T N K S `#(,C (,H . ,Part)))
		(unchain T N K S `#(,(+1, C) (,H . ,Part)))))

(func: search
    (X L) (&!fun? X) (search1 X L 1)
    (X L) (search2 X L 1))

(func: search1
    (_ '() N) 'false
    (X `(,X . ,_) N) N
    (X `(,_ . ,T)  N) (search1 X T (+1, N)))

(func: search2
    (_ '() N) 'false
    (F `(,H . ,T) N) (:- (:. F H) N (search2 F T (+1, N))))
    
