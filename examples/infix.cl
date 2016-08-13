(ql:quickload '(parmesan anaphora))
(use-package '(parmesan anaphora))

(defmacro trim (form) `(between (any whitespace) ,form (any whitespace)))
(defparser parse-int (trim (aif (many digit) (parse-integer it))))
(defparser term 
	(if (try parse-int)
	  
		 (trim (choice quoted parse-sym parse-int empty sexp))))

(defun infix-term (symbol)
	(aif (many (except symbol))
		  ()))
