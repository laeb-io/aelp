(defmodule lst_SUITE
	(export all))

(include-lib "aelp/include/aelp.macro")

(func: all () '(
    flat_1 flat_2 flat_3 flat_4 flat_5
    search_1 search_2))
        

(func: flat_1 (_) (->*
    X = '()
    Y = (~ lst:flat X)
    '() = Y))

(func: flat_2 (_) (->*
    X = '(a)
    Y = (~ lst:flat X)
    '(a) = Y))

(func: flat_3 (_) (->*
    X = '(a b c)
    Y = (~ lst:flat X)
    '(a b c) = Y))

(func: flat_4 (_) (->*
    X = '((a b) c)
    Y = (~ lst:flat X)
    '(a b c) = Y))

(func: flat_5 (_) (->*
    X = '((a b) c d)
    Y = (~ lst:flat X)
    '(a b c d) = Y))

(func: search_1 (_) (->
    2 = (~ lst:search 'b '(a b c))))

(func: search_2 (_) (->
    2 = (~ lst:search (\X.= 'b) '(a b c))))
