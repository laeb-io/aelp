(defmodule aelp
	(export all))

(include-lib "../include/aelp.macro")

(func: expand_macro:
	`(,Name ,Arg ,Body) (&* (atm? Arg) (=:= Arg ()))
		`(define-macro ,Name () (match-lambda ((,Arg $Env) ,Body)))

	`(,Name ,(= `(,H . ,_) Args) ,Body) (:-
		(all? (\X. (atm? X)) Args)
		(!! (.. H in? '(backquote list cons map =)))
			`(define-macro ,Name () (match-lambda (((list ,@Args) $Env) ,Body)))
			`(define-macro ,Name () (match-lambda ((,Args $Env) ,Body))))
	
	`(,Name ,(= `(,H . ,_) Args) ,(= `(,H' . ,_) X) ,Body) (&atm? H') (:-
		(all? (\X. (atm? X)) Args)
		(!! (.. H in? '(backquote list cons map =)))
		`(define-macro ,Name () (match-lambda (((list ,@Args) $Env) ,X ,Body)))
		`(define-macro ,Name () (match-lambda ((,Args $Env) ,X ,Body))))

	`(,Name ,Arg ,Doc ,Body) (&* (atm? Arg) (=:= Arg ()))
		`(define-macro ,Name ((doc ,Doc)) (match-lambda ((,Arg $Env) ,Body)))

	`(,Name ,(= `(,H . ,_) Args) ,Doc ,Body) (:-
		(: lists all (\X. (atm? X)) Args)
		(!! (.. H in? '(backquote list cons map =)))
		`(define-macro ,Name ((doc ,Doc)) (match-lambda
			((list ,@Args $Env) ,Body)))
		`(define-macro ,Name ((doc ,Doc)) (match-lambda
			((,Args $Env) ,Body))))
	
	`(,Name ,Args ,(= `(,H . ,_) X) ,Y ,Z) (:-guard H
		`(define-macro ,Name ((doc ,Y))
			(match-lambda ((,Args $Env) ,X ,Z)))
		`(define-macro ,Name ()
			(match-lambda ((,Args $Env) ,X) ((,Y $Env) ,Z))))
	
	`(,Name ,X . ,L) (->*
		`#(,Doc ,L') = (:- (lst? X) (~ io_lib:printable_list X)
			`#(,X ,L) `#(() (,X . ,L)))
		Keep\1 = (\
			`(,H . ,_) (&atm? H) (guard? H)
			_ 'false)
		`#(,Clause ,_) = (unchain L' 2 Keep\1)
		Clause' = (fmap (\
			`(,X ,Y) `((,X $Env) ,Y)
			`(,X ,Y ,Z) `((,X $Env) ,Y ,Z)) Clause)
		(:- (nil? Doc)
			`(define-macro ,Name () (match-lambda ,@Clause'))
			`(define-macro ,Name ((doc ,Doc)) (match-lambda ,@Clause')))))

(func: expand_\
	`(() ,Y)
		`(lambda () ,Y)

	`(,X ,Y) (&atm? X)
		`(lambda (,X) ,Y)

	`(,(= `(,H . ,_) X) ,Y) (:- (all? (\X'. (atm? X')) X)
		`(lambda ,X ,Y)
		(:-*= H
			'quote 'backquote '%> '~> 'quote 'binary 'map 'cons 'list '=
			`(match-lambda ((,X) ,Y))
			`(match-lambda (,X ,Y))))

	L (->*
		Keep\1 = (\
			`(,H . ,_) (&atm? H) (guard? H)
			_ 'false)
		`#(,L' ()) = (unchain L 2 Keep\1)
		L'' = (fmap (\
			`(,H . ,T) (&* (atm? H) (*= (car H)
				`quote 'backquote '%> '~> 'quote 'binary 'map 'cons 'list '=))
				`((,H) . ,T)
			(X) X) L')
		`(match-lambda ,@L'')))

(func: expand_func:
	`(,Name () ,Y)
		`(define-function ,Name () (lambda () ,Y))

	`(,Name ,X ,Y) (&atm? X)
		`(define-function ,Name () (lambda (,X) ,Y))

	`(,Name ,(= `(,H . ,_) X) ,Y)
		(&*= H 'quote 'backquote '%> '~> 'quote 'binary 'map 'cons 'list '=)
		`(define-function ,Name () (match-lambda ((,X) ,Y)))
	
	`(,Name ,X ,Y) (:- (all? (\X'. (atm? X')) X)
		`(define-function ,Name () (lambda ,X ,Y))
		`(define-function ,Name () (match-lambda (,X ,Y))))

	`(,Name ,X ,(= `(,H . ,_) Y) ,Z) (:-guard H
		`(define-function ,Name () (\ ,X ,Y ,Z))
		`(define-function ,Name ((doc ,Y)) (\ ,X ,Z)))

	`(,Name . ,(= `(,X ,(= `(,H . ,_) Y) ,Z ,U) T)) (:-guard H
		`(define-function ,Name ((doc ,Z)) (\ ,X ,Y ,U))
		`(define-function ,Name () (\ ,@T)))

	`(,Name ,X ,Y ,Z . ,L) (->*
		`#(,Doc ,Clause) = (:- (~ io_lib:printable_list X)
			`#(,X (,Y ,Z . ,L))
			`#(() (,X ,Y ,Z . ,L)))
		(:- (nil? Doc)
			`(define-function ,Name () (\ ,@Clause))
			`(define-function ,Name ((doc ,Doc)) (\ ,@Clause)))))

(func: expand_->
	((= `(,_ = . ,_) L) _) (expand_let L)
	((= `(,_ : . ,_) L) Env) (expand_flet L Env)
	((= `(,_ ~ . ,_) L) Env) (expand_macrolet L Env)
	(L _) `(progn ,@L))

(func: expand_->*
	((= `(,_ = . ,_) L) _) (expand_let* L)
	((= `(,_ : . ,_) L) Env) (expand_flet* L Env)
	((= `(,_ ~ . ,_) L) Env) (expand_macrolet L Env)
	(L _) `(progn ,@L))

(func: expand_->> (L Env) (->*
	`#(,X ,Y) = (find_pattern L #'make_pattern/1 ':)
	X' = (fold (\BA. (->
		B' = (~ lfe_macro:expand_expr_all B Env)
		`(,B' . ,A))) '() X)
	`(letrec-function ,X' . ,Y)))

(func: expand_let L (->*
	Keep\1 = (\
		`(,H . ,_) (guard? H)
		_ 'false)
	Stop\1 = (\
		`(,_ ,X . ,_) (&!= X '=) 'true
		_ 'false)
	`#(,L1 ,L2) = (unchain L 3 Keep\1 Stop\1)
	L1' = (fmap (\
		`(,X = ,Y) `(,X ,Y)
		`(,X = ,Y ,Z) `(,X ,Z ,Y)) L1)
	`(let ,L1' ,@L2)))
	
(func: expand_let* L (->*
	Keep\1 = (\
		`(,H . ,_) (guard? H)
		_ 'false)
	Stop\1 = (\
		`(,_ ,X . ,_) (&!= X '=) 'true
		_ 'false)
	`#(,L1 ,L2) = (unchain L 3 Keep\1 Stop\1)
	`(,L3 . ,L4) = (reverse L1)
	L5 = (:+ L3
		`(,X = ,Y) `(let ((,X ,Y)) ,@L2)
		`(,X = ,Y ,Z) `(let ((,X ,Z ,Y)) ,@L2))
	(fold (\
		(`(,X = ,Y) V2)
			`(let ((,X ,Y)) ,V2)
		(`(,X = ,Y ,Z) V2)
			`(let ((,X ,Z ,Y)) ,V2)) L5 L4)))

(func: expand_flet (L Env) (->*
	`#(,X ,Y) = (find_pattern L #'make_pattern/1 ':)
	X' = (fold (\BA. (->
		B' = (~ lfe_macro:expand_expr_all B Env)
		`(,B' . ,A))) '() X)
	`(let-function ,X' . ,Y)))

(func: expand_flet* (L Env) (->*
	`#((,H . ,T) ,Y) = (find_pattern L #'make_pattern/1 ':)
	H' = (~ lfe_macro:expand_expr_all H Env)
	X = `(let-function (,H') . ,Y)
	(fold (\BA. (->
		B' = (~ lfe_macro:expand_expr_all B Env)
		`(let-function (,B') ,A))) X T)))

(func: expand_macrolet (L Env) (->*
	`#(,X ,Y) = (find_pattern L #'make_pattern2/1 '~)
	X' = (fold (\BA. (->
		B' = (~ lfe_macro:expand_expr_all B Env)
		`(,B' . ,A))) '() X)
	`(let-macro ,X' . ,Y)))

(func: expand_:+ `(,H . ,T) (->*
	Keep\1 = (\
		`(,H' . ,_) (&atm? H') (guard? H')
		_ 'false)
	Stop\1 = (\
		`(-# . ,_) 'true
		`(catch . ,_) 'true
		`(-$ . ,_) 'true
		`(after . ,_) 'true
		_ 'false)
	(:+ (unchain T 2 Keep\1 Stop\1)
		`#(,L ())
			`(case ,H ,@L)
		`#(,L (,H' . ,T')) (&*= H' '-# 'catch) (->*
			X = (:- (nil? L) () `((case ,@L)))
			`#(,Catch ,After) = (expand_catch T')
			After' = (:- (nil? After) () `((after ,After)))
			`(try ,H ,@X (catch ,@Catch) ,@After'))
		`#(,L (,H' ,Y)) (&*= H' '-@ 'after) (->
			X = (:- (nil? L) () `((case ,@L)))
			`(try ,H ,@X (after ,Y))))))

(func: expand_catch L (->*
	Keep\1 = (\
		`(,H' . ,_) (&atm? H') (guard? H')
		_ 'false)
	Stop\1 = (\
		`(-$ . ,_) 'true
		`(after . ,_) 'true
		_ 'false)
	`#(,L1 ,L2) = (:+ (unchain L 2 Keep\1 Stop\1)
		`#(,X (,H' ,Y)) (&*= H' '-$ 'after)
			`#(,X ,Y)
		Else Else)
	L1' = (fmap (\
		`(,H . ,T) (&* (atm? H) (&& (lst? H) (*= (hd H)
			'quote 'backquote '%> '~> 'quote 'binary 'map 'cons 'list '=)))
			`((tuple _ ,H _) . ,T)
		`((,X) . ,T)
			`((tuple ,X _ _) . ,T)
		`((,X ,Y) . ,T)
			`((tuple ,X ,Y _) . ,T)
		`((,X ,Y ,Z) . ,T)
			`((tuple ,X ,Y ,Z) . ,T)) L1)
	`#(,L1' ,L2)))

(func: expand_? L (->*
	Keep\1 = (\
		`(,H' . ,_) (&atm? H') (guard? H')
		_ 'false)
	Stop\1 = (\
		`(-@ . ,_) 'true
		`(after . ,_) 'true
		_ 'false)
	(:+ (unchain L 2 Keep\1 Stop\1)
		`#(,L' ())
			`(receive ,@L')
		`#(,L (,_  ,X ,Y)) (&atm? X)
			`(receive ,@L (after ,(time_conv X) ,Y))
		`#(,L' (,_  ,Ms ,Body))
			`(receive ,@L' (after ,Ms ,Body)))))

(func: expand_~ `(,H . ,T) (->*
	Str = (atl H)
	`(,M ,F) = (:+ (~ string:split Str ":")
		X (& (2lst? X)) X
		X (& (1lst? X)) (->
			`(,X ,Y) = (~ string:split Str ".")
			`(,(++ "Elixir." X) ,Y)))
	F' = (lta! F)
	M' = (lta! M) (->
		upper_char? ~ (X) `(.. (.. ,X >= #\A) && (.. ,X <= #\Z))
		(:- (all? (\X. (upper_char? X)) M)
			`(call (,M') ',F' ,@T)
			`(: ,M' ,F' ,@T)))))

(func: time_conv A (->
	`(,Suffix . ,N) = (reverse (atl A))
	(:-* (nil? N) (!! (all? (\X. (digi? X)) N)) A (->*
	Scale = (:+ Suffix
		#\s 1000
		#\m #.(* 60 1000)
		#\h #.(* 60 60 1000)
		#\d #.(* 24 60 60 1000))
	N' = (lti (reverse N))
	(.. N' * Scale)))))

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

(func: find_pattern (L F Sep)
	(find_pattern L () () F Sep))
	
(func: find_pattern
	(() _ Part2 _ _)
		`#(,Part2 ())
	
	(`(,Name ,X . ,T) () Part2 F Sep) (&= Sep X)
		(find_pattern2 T `#(,Name ()) Part2 F Sep)

	(`(,Name ,X . ,T) Part1 Part2 F Sep) (&= Sep X)
		(find_pattern2 T `#(,Name ()) `(,(:. F Part1) . ,Part2) F Sep)

	(`(,X . ,T) Part1 Part2 F Sep) (&= Sep X)
		(find_pattern2 T Part1 Part2 F Sep)

	(L Part1 Part2 F _)
		`#((,(:. F Part1) . ,Part2) ,L))

(func: find_pattern2
	(`(,Args ,(= `(,H . ,_) X) ,Y . ,T) `#(,Name ,P) Part2 F Sep) (:-guard H
		(find_pattern T `#(,Name ((,Args ,X ,Y) . ,P)) Part2 F Sep)
		(find_pattern `(,Y . ,T) `#(,Name ((,Args ,X) . ,P)) Part2 F Sep))

	(`(,Args ,X . ,T) `#(,Name ,P) Part2 F Sep)
			(find_pattern T `#(,Name ((,Args ,X) . ,P)) Part2 F Sep))

(func: make_pattern `#(,Name ,L) `(,Name (\ ,@(~ lists:append (reverse L)))))

(func: make_pattern2 `#(,Name ,L) (->
	L' = (fmap (\
		`(,(= `(,H . ,_) X) ,Y) (:-
			(all? (\X'. (atm? X')) X)
			(!! (*= H 'backquote '%> '~> 'list 'cons 'binary 'map '=))
			`(((list ,@X) $Env) ,Y)
			`((,X $Env) ,Y))
		`(,(= `(,H . ,_) X) ,Y ,Z) (:-
			(all? (\X'. (atm? X')) X)
			(!! (*= H 'backquote '%> '~> 'list 'cons 'binary 'map '=))
			`(((list ,@X) $Env) ,Y ,Z)
			`((,X $Env) ,Y ,Z))
		`(,X ,Y) (&* (atm? X) (nil? X))
			`((,X $Env) ,Y)
		`(,X ,Y ,Z) (&atm? X)
			`((,X $Env) ,Y ,Z)) L)
	`(,Name (match-lambda ,@(reverse L')))))

(macro: upper_char? (X) `(.. (.. ,X >= #\A) && (.. ,X <= #\Z)))

(func: camel_to_snake
	`(,H . ,T) (& (upper_char? H)) (->>
		rec : (()) ()
			: `(,H . ,T) (& (upper_char? H))
				`(#\_ ,(to_lower_char H) . ,(rec T))
			: `(,H . ,T)
				`(,H . ,(rec T))
		`(,(to_lower_char H) . ,(rec T)))

	((~> H (T bs))) (& (upper_char? H)) (->>
		rec : (#"" Acc)
				(ltb (reverse Acc))
			: ((~> H (T bs)) Acc) (& (upper_char? H))
				(rec T `(,(to_lower_char H) . ,Acc))
			: ((~> H (T bs)) Acc)
				(rec T `(,H . ,Acc))
		(rec T `(,(to_lower_char H)))))

(func: to_lower_char (X) (+ X 32))
