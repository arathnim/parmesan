(ql:quickload '(parmesan anaphora))
(use-package '(parmesan anaphora))

(defmacro trim (form) `(between (any whitespace) ,form (any whitespace)))
(defparser parse-sym (aif (seq (choice sym letter) (any (choice sym letter digit))) (intern (string-upcase it))))
(defparser parse-int (aif (many digit) (parse-integer it)))
(defparser str-term  (if (try "\\\"") (progn (str "\\\"") "\"") (except "\"")))
(defparser quoted    (between "\"" (any str-term) "\""))
(defparser empty     (if (str "()") nil))
(defparser term      (trim (choice quoted parse-sym parse-int empty sexp)))
(defparser sexp      (trim (between "(" (any* term) ")")))