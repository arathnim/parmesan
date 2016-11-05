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
   
   Or we can walk the code in 'parse' and 'defparser' and figure it all out
   dynamically. This is probably better for optimization, and allows
   for a lot of fancy macro tools.

   Make sure that all the parser functions are inlined, at the very least.

   the first still makes it possible to use 'or' and 'and' normally!

   (seq a <- "foo"
        b <- "bar"
        c <- "baz"
       (list a b c))

   hex -> rgb example

   (defun parse-hex-color (str)
      (parse str
         (optional "#")
         (seq (r <- times 2 hex-digit)
              (g <- times 2 hex-digit)
              (b <- times 2 hex-digit)
              (make-rgb r g b))))
   
   You can't 'apply #'or' or anything, because that one's a macro, but it's easy
   to work around it to make parsers modifiable dynamically.

error handling
   error goes in the value slot, a list with all the possible characters, or a formatted string that has the parser name, and the index
   col and index
   choice returns the error from the attempt with the largest index!
      make sure you combine the errors with the same index
