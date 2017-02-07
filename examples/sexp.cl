(ql:quickload '(parmesan))
(in-package parmesan)

;; Simple s-expr parser, meant only as an example.
;; Probably not even close to any kind of standard, but whatever.

(defparser symbol-char () 
  (one-of "~!@#$?%^&*-_=+<>,./\\"))

(defparser whitespace ()
	(choice #\Tab #\Space #\Newline))

(defparser digit () (range #\0 #\9))
(defparser letter () (choice (range #\a #\z) (range #\A #\Z)))

(defparser between (start end parser)
	(seq start (result <- parser) end
		result))

(defparser ignore-whitespace (form) 
  (between (any whitespace) (any whitespace) form))

;; according to the standard, symbols must start with either a letter or normal symbol characer, 
;; and then any number of letters, symbol characters, or numbers
(defparser parse-symbol ()
  (first-char <- (choice symbol-char letter))
  (other-char <- (any (choice symbol-char letter digit)))
  (intern (string-upcase (cat first-char other-char))))

(defparser parse-int ()
  (n <- (many digit))
  (parse-integer n))

(defparser parse-list ()
  (ignore-whitespace (between "(" ")" (any expression))))

(defparser parse-quote ()
  #\' (exp <- expression)
 (list 'quote exp))

(defparser expression ()
  (ignore-whitespace (choice parse-symbol parse-int parse-list parse-quote)))

;; usage
;; (parse-from-stream *standard-input* expression)
;; (parse "(a b c)" expression)
