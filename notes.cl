;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

Parmesan will only combine chars
	(many (one-of "aeiou"))
	one-of returns a single char, not a one-char string, Which many concatenates.
	many might recieve anything, so it does a normal list collection, and if the
	whole thing is characters, they are concatenated.

functional approach
	Where each one is an almost-function and returns (value ind status)
	However, this makes it *really* difficult to access the value,
	could I construct a macro to allow normal access to the value in its scope?

	(mess-with-the-value
		(many hex-digits)
		(if it (parse-integer it :radix 16))) 

	The primary benifit is it eliminates the need for a special stack, values can
	be passed normally. However, the operators will still need to be macros, so
	"foo" can be transformed into (string "foo") and similar.
