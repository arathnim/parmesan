# Parmesan
## parser combinators for Common Lisp

Parmesan is a little parser combinator library, based primarily on Parsec, with a few changes to make it more lispy.
It differs from Parsec and other CL parser combinator libraries by being focused on simplicity and providing elegant, intuitive parsers.

Other CL parser combinator libraries prefix the parsers with `=` or `.`, but Parmesan uses single-word parser names like `choice` and `many`.  
Parsers are treated as both functions and variables, so you don't need parens around parsers with no arguments.

`seq` and `defparser` use a syntax similar to haskell do-notation, using a left-arrow for binding and extracting parser results.
character and string parsers can be invoked just by using literal chars and strings.

For example, this line of Haskell from [Write Yourself a Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours),
which parses a simple string without escapes:

```Haskell
parseString = do
              char '"'
              x <- many (noneOf "\"")
              char '"'
              return $ String x
```

could be written in parmesan as:

```cl
(defparser parse-string ()
  #\"
  (s <- (any (none-of #\")))
  #\"
  s)
```

## A couple parsers

`choice` takes any number of forms, and returns the first one that succeeds.
So the form `(choice "foo" "bar")` parses either the string "foo" or the string "bar".

```cl
(parse "foo" (choice "foo" "bar")) => "foo"
(parse "bar" (choice "foo" "bar")) => "bar"
(parse "baz" (choice "foo" "bar"))
 => error: expected either "foo" or "bar" at position 0
```

`any` parses zero or more of the same form, similar to `*` in regular expressions.

```cl
(parse "aaabbb" (any "a")) => "aaa"
```

`many` parses one or more of the same form

```cl
(parse "aaabbb" (many "a")) => "aaa"
(parse "bbb" (many "a")) => nil
```

`seq` matches each form sequentially, and returns a list of the matched forms

```cl  
(defparser seq-test () (seq "a" (choice "a" "b") (many "c")))
(parse "abccc" (seq-test)) => ("a" "b" "ccc")
```

See sexp.cl for some more involved examples.

## dependencies and installation

Parmesan requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
to install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow the instructions there.
Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable when you start your interpreter.

To use parmesan, clone the repo into `~/quicklisp/local-projects`, and run `(ql:quickload 'parmesan)`.

## Thanks

Thanks to [kori](https://github.com/kori), who came up with the fun name for this project, and
the developers who worked on Parsec, as well as the authors of other lisp parser combinator libraries.
