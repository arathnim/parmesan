(asdf:defsystem "parmesan"
	:serial t
	:version "0.1"
	:author "Dylan Ball <arathnim@gmail.com>"
	:maintainer "Dylan Ball <arathnim@gmail.com>"
	:description "Parmesan"
	:long-description "parser combinator macros, lisp-style"
	:depends-on (alexandria iterate anaphora)
	:components ((:file "parmesan")))
