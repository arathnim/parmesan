;; author *this* => ("Dylan Ball" "Arathnim@gmail.com")
(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(defpackage parmesan
	(:use cl alexandria iterate anaphora)
	(:export 
		:parse :pretty-error :call-with-context :return-with-context
		:defparser :seq :choice :one-of :none-of :many :sep-many :many+ :sep-many+
		:sep-num :num :par :ret :str :skip :consume-byte :restr :option :between :any-char
		:sym :letter :digit :hex-digit :whitespace :sep))
(in-package parmesan)

(defvar parse-stack nil) ;; (str)
(defvar match-stack nil) ;; (new-ind success)/test-ind

(defun get-str ()
	(if parse-stack
		 (first parse-stack)
		 (error "Tried to access the parse-stack without any frames left")))

(defun get-ind ()
	(if match-stack
		 (first match-stack)
		 (error "Tried to access the match-stack without any frames left")))

(defun ind ()
	(let ((v (first match-stack)))
		  (if (listp v) (first v) v)))

;; primitive parser for basic string matching
(defun str-match (src test ind)
	(if (>= (length src) (+ ind (length test)))
		 (iter (for x from ind to (+ ind (length test)))
				 (for y in-vector test)
				 (if (not (equal (aref src x) y))
					  (leave nil))
				 (finally (return t)))
		 nil))

(defun apply-pred (pred list)
	(iter (for x in list)
			(if (not (funcall pred x)) (leave))
			(finally (return t))))

(defun merge-str (list)
	(if (apply-pred #'stringp list)
		 (format nil "~{~a~}" list)
		 list))

(defmacro parse (string exp)
  `(progn (push ,string parse-stack)
	  		 (push 0 match-stack)
			 (let ((res ,exp) (rv (progn (pop parse-stack) (pop match-stack))))
					;; (print (list 'return-value (pop match-stack)))
					(pop match-stack)
					res)))

(defun bounded-area (str ind range)
	(let ((a (- ind range)) (b (+ ind range)) (len (length str)))
		  (when (< a 0) (incf ind (- 0 a)) (setf a 0))
		  (when (> b len) (setf b len))
		  (list (subseq str a b) (- ind a))))

(defun build-pointer (ind)
	(format nil "~{~a~}^" (iter (repeat ind) (collect " "))))

(defun pretty-error (str)
	(let ((area (bounded-area (get-str) (ind) 12)))
		  (error "~%~a~%~a~%~a" str (first area) (build-pointer (second area)))))

(defmacro call-with-context (exp context)
	(if (stringp exp) (setf exp `(str ,exp)))
	(if (symbolp exp) (setf exp `(,exp)))
	(if (numberp exp) (setf exp `(str (string (code-char ,exp)))))
	(if (characterp exp) (setf exp `(string ,exp)))
	(with-gensyms (res con)
	 `(progn (push ,context match-stack)
			   (let ((,res ,exp) (,con (pop match-stack)))
				     (pop match-stack)
					  (list ,res ,con)))))

(defun make-case-clauses (block-name test-var list)
	(iter (for x in list)
			(if (eq (car x) 'otherwise)
				 (collect
					`(return-from ,block-name (progn ,@(cdr x))))
				 (collect
			  		`(if (equalp ,(car x) ,test-var) 
					  	  (return-from ,block-name (progn ,@(cdr x))))))))

(defmacro case! (exp &rest forms)
	(with-gensyms (v b)
	  `(let ((,v ,exp))
			(block ,b ,v ,@(make-case-clauses b v forms)))))

(defmacro return-with-context (ret context)
  `(progn (push ,context match-stack)
			 ,ret))

(defmacro defparser (name &rest forms)
	`(defun ,name () ,@forms))

(defun cat (list)
	(format nil "~{~a~}" list))

(defmacro fail ()
	`(return-with-context nil (list (get-ind) nil)))

(defmacro pass (form)
	`(return-with-context (first ,form) (second ,form)))

;; choice ~ in the matching operation, returns the first form that consumes input
;; many   ~ matches zero or more of the next form
;; many+  ~ matches one or more of the next form
;; one-of ~ matches one character from the given string
;; skip   ~ consumes input, doesn't return the value
;; count  ~ parses x occurances of y
;; option ~ tries x, if it fails, parse y
;; seq    ~ matches each form sequentially
;; str    ~ explicitly matches a string

(defmacro seq (form &rest aux)
	(if (first aux)
		(with-gensyms (ind res inner)
			`(let* ((,ind (get-ind)) (,res (call-with-context ,form ,ind)))
				(if (second (second ,res))
					 (let ((,inner (call-with-context (seq ,(car aux) ,@(cdr aux)) (first (second ,res)))))
					  	 (if (second (second ,inner)) 
							  (return-with-context 
									(if (first ,inner)
								 		 (if (first ,res)
											  (append (list (first ,res)) (first ,inner))
											  (first ,inner))
							  			 (if (first ,res)
											  (list (first ,res))
											  nil)) 
							  	   (second ,inner))
							  (fail)))
					 (fail))))
		(with-gensyms (str ind res)
			`(let* ((,ind (get-ind)) (,res (call-with-context ,form ,ind)))
				(if (second (second ,res))
					 (return-with-context (if (first ,res) (list (first ,res))) (list (first (second ,res)) t))
					 (fail))))))

(defmacro choice (form &rest aux)
	(if (first aux)
		 (with-gensyms (ind res inner)
			`(let* ((,ind (get-ind)) (,res (call-with-context ,form ,ind)))
				(if (second (second ,res))
					 (return-with-context (first ,res) (second ,res))
					 (let ((,inner (call-with-context (choice ,(car aux) ,@(cdr aux)) (first (second ,res)))))
					  	 (if (second (second ,inner)) 
							  (return-with-context (first ,inner) (second ,inner))
							  (return-with-context nil (list ,ind nil)))))))
		 (with-gensyms (str ind res)
			`(let* ((,ind (get-ind)) (,res (call-with-context ,form ,ind)))
				(if (second (second ,res))
					 (return-with-context (if (first ,res) (first ,res)) (list (first (second ,res)) t))
					 (return-with-context nil (list ,ind nil)))))))

(defmacro one-of (form)
	(with-gensyms (src ind i str)
		`(let ((,str ,form) (,ind (get-ind)) (,src (get-str)))
			(if (< ,ind (length ,src))
				 (iter (for ,i in-string ,str)
						 (if (equalp ,i (aref ,src ,ind))
						 	  (leave (return-with-context (string ,i) (list (+ ,ind 1) t))))
						 (finally (return (return-with-context nil (list ,ind nil)))))
				 (return-with-context nil (list ,ind nil))))))

(defmacro none-of (form)
	(with-gensyms (src ind i str)
		`(let ((,str ,form) (,ind (get-ind)) (,src (get-str)))
			(if (< ,ind (length ,src))
				 (iter (for ,i in-string ,str)
						 (if (equalp ,i (aref ,src ,ind))
						 	  (leave (return-with-context nil (list ,ind nil))))
						 (finally (return (return-with-context (string (aref ,src ,ind)) (list (+ ,ind 1) t)))))
				 (return-with-context nil (list ,ind nil))))))

(defmacro many (form)
  `(aif (sep-many ,form) (merge-str it)))

(defmacro sep-many (form)
	(with-gensyms (src ind v i s acc z)
		`(let ((,ind (get-ind)) (,src (get-str)))
			(iter (with ,z = ,ind)
					(for (,v (,i ,s)) = (call-with-context ,form ,z))
					(if ,v (collect ,v into ,acc))
					(if (not ,s)
						 (leave (return-with-context (if ,acc ,acc) (list ,z t)))
						 (setf ,z ,i))))))

(defmacro many+ (form)
  `(aif (sep-many+ ,form) (merge-str it)))

(defmacro sep-many+ (form)
	(with-gensyms (src ind v i s acc z c)
		`(let ((,ind (get-ind)) (,src (get-str)))
			(iter (with ,z = ,ind)
					(with ,c = 0)
					(for (,v (,i ,s)) = (call-with-context ,form ,z))
					(incf ,c)
					(if ,v (collect ,v into ,acc))
					(if (not ,s)
						 (if (> ,c 1) 
						 	  (leave (return-with-context ,acc (list ,z t)))
							  (leave (return-with-context nil (list ,ind nil))))
						 (setf ,z ,i))))))

(defmacro sep-num (n form)
	(with-gensyms (src ind v i s acc z c)
		`(let ((,ind (get-ind)) (,src (get-str)))
			(iter (with ,z = ,ind)
					(with ,c = 0)
					(for (,v (,i ,s)) = (call-with-context ,form ,z))
					(incf ,c)
					(if ,v (collect ,v into ,acc))
					(if (not ,s)
						 (leave (fail))
						 (setf ,z ,i))
					(if (eql ,c ,n)
						 (leave (return-with-context (if ,acc ,acc) (list ,z t))))))))

(defmacro num (n form)
	`(aif (sep-num ,n ,form) (merge-str it)))

(defmacro sep (form sep-by)
	(with-gensyms (src ind v i s acc z x)
		`(let ((,ind (get-ind)) (,src (get-str)))
			(iter (with ,z = ,ind)
					(with ,acc = nil)
					(for (,v (,i ,s)) = (call-with-context ,form ,z))
					(if (not ,s)
						 (leave (return-with-context (if ,acc ,acc) (list ,z t)))
						 (progn
							(appendf ,acc (list ,v))
							(let ((x (call-with-context ,sep-by ,i)))
								  (if (second (second x))
										(setf ,z (first (second x)))
										(leave (return-with-context ,acc (list ,i t)))))))))))

(defmacro par (form)
	(with-gensyms (res)
		 `(let ((,res (call-with-context ,form (get-ind))))
			(if (second (second ,res))
				 (return-with-context (first ,res) (second ,res))
				 (return-with-context nil (list (get-ind) nil))))))

(defmacro ret (form)
	(with-gensyms (res ing)
		 `(let ((,ing (pop match-stack)) (,res (call-with-context ,form (get-ind))))
			(if (second (second ,res))
				 (return-with-context (first ,res) (second ,res))
				 (return-with-context nil (list (get-ind) nil))))))

(defmacro str (form)
	(with-gensyms (str src ind)
 		 `(let ((,str ,form) (,ind (get-ind)) (,src (get-str)))
			(if (str-match ,src ,str ,ind)
				 (return-with-context ,str (list (+ (length ,str) ,ind) t))
				 (return-with-context nil (list ,ind nil))))))

(defmacro skip (form)
	(with-gensyms (res)
	  `(let ((,res (call-with-context ,form (get-ind))))
			  (if (second (second ,res))
					(return-with-context nil (second ,res))
					(return-with-context nil (list (get-ind) nil))))))

(defmacro consume-byte ()
	(with-gensyms (str ind)
 		 `(let ((,str (get-str)) (,ind (get-ind)))
			(if (< ,ind (length ,str))
				 (return-with-context (string (aref ,str ,ind)) (list (+ 1 ,ind) t))
				 (return-with-context nil (list ,ind nil))))))

(defmacro restr (form)
	(with-gensyms (res ind)
	 `(let* ((,ind (get-ind)) (,res (call-with-context ,form ,ind)))
			  (if (second (second ,res))
					(return-with-context (subseq (get-str) ,ind (first (second ,res))) (second ,res))
					(return-with-context nil (list ,ind nil))))))

(defmacro option (form aux)
	(with-gensyms (res ind)
	 `(let* ((,ind (get-ind)) (,res (call-with-context ,form ,ind)))
			  (if (second (second ,res))
					(return-with-context nil (list ,ind nil))
					(par ,aux)))))

(defmacro between (before form after)
	`(car (seq (skip ,before) ,form (skip ,after))))

(defparser any-char (consume-byte))
(defparser sym (one-of "~!@#$%^&*-_=+<>,./\\"))
(defparser letter (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(defparser digit (one-of "0123456789"))
(defparser hex-digit (one-of "0123456789abcdef"))
(defparser whitespace (one-of (coerce '(#\Space #\Tab #\Newline) 'string)))
