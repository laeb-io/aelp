(defmodule aelisp_SUITE
	(export all))

(include-lib "aelisp/include/aelisp.macro")

(func: all () '(expand_let expand_let_string expand_let*))

(func: expand_let (_) (->*
	R = (~ aelisp:expand_let '(X = Y Z))
	'(let ((X Y)) Z) = R))

(func: expand_let_string (_) (-> (catch (->*
	R = (catch (~ aelisp:expand_let '(X = "abc" Z)))
	'(let ((X "abc")) Y) = R))
	#(skip "TODO")))

(func: expand_let* (_) (->*
    Original = '(A = B C = D (+ A B)) 
    Expected = '(let ((A B)) (let ((C D)) (+ A 'B)))
    Actual = (~ aelisp:expand_let* Original)
    (assert= Expected Actual)))
