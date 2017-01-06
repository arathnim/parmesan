(asdf:defsystem "parmesan"
	:serial t
	:version "0.1"
	:author "Dylan Ball <arathnim@gmail.com>"
	:maintainer "Dylan Ball <arathnim@gmail.com>"
	:description "Parmesan"
	:long-description "parser combinators, lisp-style"
	:depends-on (alexandria iterate anaphora destructuring-match)
	:components ((:file "parmesan")))
