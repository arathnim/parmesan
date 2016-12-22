(ql:quickload '(parmesan))
(use-package '(parmesan))

(defparser ignore-whitespace (form) 
	(between (any whitespace) (any whitespace) form))

(defparser symbol-char () 
	(one-of "~!@#$?%^&*-_=+<>,./\\"))

;; according to the standard, symbols must start with either a letter or normal symbol characer, 
;; and then any number of letters, symbol characters, or numbers
(defparser symbol ()
	(seq (first-char <- (choice symbol-char letter))
		  (other-char <- (any (choice symbol-char letter digit)))
		  (intern (string-upcase (cats first-char other-char)))))

(defparser int ()
	(seq (n <- many digit)
		  (parse-integer n)))

(defparser list ()
	(ignore-whitespace (between "(" ")" (any expression))))

(defparser quote ()
	(seq #\'
		  (exp <- expression)
		 `(quote ,exp)))

(defparser expresssion
	(ignore-whitespace (choice quoted-string symbol int list quote)))

;; usage
(parse-from-stream *standard-input* expression)
(parse string expression)
