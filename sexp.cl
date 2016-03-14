(load "parmesan.cl")
(in-package parmesan)

(defparser symb   (aif (many+ letter) (intern (string-upcase it))))
(defparser int    (aif (many+ digit) (parse-integer it)))
(defparser quoted (between "\"" (many (none-of "\"")) "\""))
(defparser term   (between (many whitespace) (choice quoted symb int sexp) (many whitespace)))
(defparser sexp   (between (many whitespace) (between "(" (sep-many term) ")") (many whitespace)))
