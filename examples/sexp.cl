(ql:quickload '(parmesan))
(use-package '(parmesan))

;; Simple s-expr parser, meant only as an example.
;; Probably not even close to any kind of standard, but whatever.

(defparser ignore-whitespace (form) 
  (between (any whitespace) (any whitespace) form))

;; technically, symbol characters should be
(defparser symbol-char () 
  (one-of "~!@#$?%^&*-_=+<>,./\\"))

;; according to the standard, symbols must start with either a letter or normal symbol characer, 
;; and then any number of letters, symbol characters, or numbers
(defparser symbol ()
  (first-char <- (choice symbol-char letter))
  (other-char <- (any (choice symbol-char letter digit)))
  (intern (string-upcase (cat first-char other-char))))

(defparser int ()
  (n <- (many digit))
  (parse-integer n))

(defparser list ()
  (ignore-whitespace (between "(" ")" (any expression))))

(defparser quote ()
  #\' (exp <- expression)
 (list 'quote exp))

(defparser expresssion
  (ignore-whitespace (choice quoted-string symbol int list quote)))

;; usage
(parse-from-stream *standard-input* expression)
(parse "(a b c)" expression)
