(ql:quickload '(parmesan anaphora))
(use-package '(parmesan anaphora))

(defparser parse-sym (aif (many+ letter) (intern (string-upcase it))))
(defparser parse-int (aif (many+ digit) (parse-integer it)))
(defparser quoted    (between "\"" (many (choice (if (par "\\\"") "\"") (none-of "\""))) "\""))
(defparser term      (between (many whitespace) (choice quoted parse-sym parse-int sexp) (many whitespace)))
(defparser sexp      (between (many whitespace) (between "(" (sep-many term) ")") (many whitespace)))
