# Interface styles

The biggest issue is the fact that things are collected into lists of characters when we want strings.

## Macro interface style A

Entry points for parsing are only `defparser`, and `parse`. 
Parsers exist in a separate namespace from functions and variables.
`defparser` has an implicit `seq`, so bindings can occur there. Plays nice with Aphasia.

All `seq` variants can have nested bindings, as well as bindings deeper in the form. 
Goes up to the nearest `seq`, if those are nested.

This style is very fast, especially if the core parsers are implemented internally.
Control at the defparser/parse level allows for as much optimization as desired, including opportunistic inlining, loop unrolling, ect.

Edge case with parsers that accept lists, have to be implemented internally, or with `apply-parser`, which is very difficult to
implement correctly, and will probably make things slower.

```
(defparser int ()
  (n <- (many digit))
  (parse-integer n))

(defparser symbol ()
  (first-char <- (choice symbol-char letter))
  (other-char <- (any (choice symbol-char letter digit)))
  (intern (string-upcase (cats first-char other-char))))

(defparser ignore-whitespace (form) 
  (between (any whitespace) (any whitespace) form))
```

### Options & Variants

Can be used with or without allowing arguments to defparser forms. Not allowing them
means core forms will have to be implemented separately.

Core parsers are very difficult to implement in defparser forms, especially seq and choice, and would likely be slower that way.

The first implementation of `seq` scans for arrow bindings, if any are found, 
the last form evaluates as normal code, which determines only the value of the result,
it can't modify the position, or call an error.
This means lower-level forms that need direct access will need to be implemented elsewhere,
so this style loses points in simplicity and orthogonality.

The second is the same as the first, but control at the end of the seq is explicit, using `pass`, `error`,
or modifying the objects directly. Allows for more control over what the parser returns,
at the cost of brevity.

The third is split between the first and the second, `seq` behaves as in the first, `seq!` (and `defparser!`)
gives the full object for bindings. 
For a possible variant on this implementation, `seq!` is actually just a progn, allows for normal usage of `apply-parser`.
Only makes sense under implementations that don't use internal core parser definitions, which removes the need for `apply-parser`.

## Macro interface style B

Extensive handling by defparser/parse 'flows' together normal code and parsers. 
Normal code gets access to the values, can control parsing via special error forms.
Status and index are floated past the value modifications.

```
(defparser int ()
  (let ((n (many digit)))
    (when n
	   (parse-integer n))))
```

Main disadvantage is that you can't pass literal nil's, which makes everything harder. 
Also, this is really, really hard to implement correctly.

## Function interface style A

Pure functions, no messing around with internal macros, expect for things that are explicitly macros, like defparser and seq.
Main disadvantage is speed, although having the core pure helps with keeping the interface clean, since everything is apply/lambda.

```
(defun choice (&rest rest)
  (lambda ()
    ))
```

# Error handling styles

## Function dispatch style

Each error returns a value in the normal value slot that serves as a function with arguments. nil means a non-error failure, and should
generally be ignored by choice and similar functions, when figuring out which error(s) to return. For example, the normal string parser
returns a nil error when the input doesn't start with a double quote, but gives `(standard-error "unknown escape character: ~a" "d")`
when given the input `\d` somewhere in the string.

A more nuanced error system could be added on as well, with numerical levels, or symbols to clarify error type or rank.

More on scanning parsing and error-resistant parsing later.

## Lambda style

They all return lambdas that act on other lambdas. Very slow, this is a bad idea.