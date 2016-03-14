# Parmesan
## parser combinator macros for Common Lisp

This is a little parser combinator library, with a few changes to make it more lispy. 
It differs from Parsec and [parseltounge](https://github.com/VincentToups/parseltongue) 
by being based on return values, opposed to binding the values sequentially.

For example, this line of Haskell from [Write Yourself a Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)

    parseString = do
                  char '"'
                  x <- many (noneOf "\"")
                  char '"'
                  return $ String x
                  
Would be written in parmesan as

    (defparser parse-string 
      (between "\"" (many (none-of "\"")) "\""))
      
## A couple parsers

`choice` takes a list of forms, and returns the first one that succeeds

    (parse "a" (choice "a" "b")) => "a"
    (parse "b" (choice "a" "b")) => "b"
    (parse "c" (choice "a" "b")) => nil
    
`many` parses zero or more of the same form, same as in parsec

    (parse "aaabbb" (many "a")) => "aaa"

`many+` parses one or more of the same form

    (parse "aaabbb" (many+ "a")) => "aaa"
    (parse "bbb" (many+ "a")) => nil
    
`seq` matches each form sequentially, and returns a list of the matched forms
    
    (defparser seq-test (seq "a" (choice "a" "b") (many "c")))
    (parse "abccc" (seq-test)) => ("a" "b" "ccc")
    
see sexp.cl for some more involved examples.

## dependencies and installation

Parmesan requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
to install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow the instructions there.
Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable when you start your interpreter.

To use parmesan, just `load parmesan.cl` in your interpreter, `use` the package, and the macros will be avaliable.

## Thanks

Thanks to [kori](https://github.com/kori), who came up with the fun name for this project.
