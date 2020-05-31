(defmodule str
	(export all))

(include-lib "../include/aelp.macro")

(macro: upper_char? (X) `(.. (.. ,X >= #\A) && (.. ,X <= #\Z)))

(macro: lower_char? (X)
	`(.. (.. ,X >= #\a) && (.. ,X <= #\z)))

(func: snake_to_camel
	`(,H . ,T) (& (lower_char? H)) (->>
		rec : (()) ()
			: `(#\_ ,H . ,T)
				`(,(to_upper_char H) . ,(rec T))
			: `(,H . ,T)
				`(,H . ,(rec T))
		`(,(to_upper_char H) . ,(rec T)))

	((~> H (T bs))) (& (lower_char? H)) (->>
		rec : (#"" Acc)
				(ltb (reverse Acc))
			: ((~> #\_ H (T bs)) Acc)
				(rec T `(,(to_upper_char H) . ,Acc))
			: ((~> H (T bs)) Acc)
				(rec T `(,H . ,Acc))
		(rec T `(,(to_upper_char H)))))

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

(func: to_upper_char (X) (- X 32))
(func: to_lower_char (X) (+ X 32))
