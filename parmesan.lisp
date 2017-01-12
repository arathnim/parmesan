;; author *this* => ("Dylan Ball" "Arathnim@gmail.com")

(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(ql:quickload '(alexandria iterate anaphora destructuring-match) :silent t)
(defpackage parmesan
   (:use cl alexandria iterate anaphora destructuring-match))

(in-package parmesan)

(defvar *source* nil)
(defvar *index* 0)

(defstruct (source)
   value
   (cached-string "")
   (max-index -1))

(defun get-character (n)
   (if (stringp (source-value *source*))
       (char (source-value *source*) n)
       (if (> n (source-max-index *source*))
           (iter (until (eql (source-max-index *source*) n))
                 (append-character (read-char (source-value *source*)))
                 (finally (return (char (source-cached-string *source*) n))))
           (char (source-cached-string *source*) n))))

(defun append-character (c)
   (setf (source-cached-string *source*)
         (concatenate 'string
            (source-cached-string *source*)
            (string c)))
   (incf (source-max-index *source*)))

(declaim (inline error?))
(defstruct (parser-error (:conc-name error-) (:predicate error?))
   type index args)

(defun kitten (&rest list)
   (format nil "~{~a~}" list))

(defun kitten* (list)
   (format nil "~{~a~}" list))

(defun cat (&rest list)
   (format nil "~{~a~}" 
      (remove-if-not (lambda (x) x) list)))

(defun cat* (list)
   (format nil "~{~a~}" 
      (remove-if-not (lambda (x) x) list)))

(defun merge-characters (list)
   (if (and (listp list) (every #'characterp list))
       (kitten* list)
       list))

(declaim (inline pass))
(defun pass (value n)
   (incf *index* n)
   value)

(declaim (inline fail))
(defun fail (type args &optional (index *index*))
   (make-parser-error :type type :args args :index index))

(defmacro on-success (exp &rest body)
   (with-gensyms (r)
      `(let ((,r ,exp)) 
         (if (error? ,r)
             ,r
             (progn ,@body)))))

(defmacro on-failure (exp &rest body)
   (with-gensyms (r)
      `(let ((,r ,exp)) 
         (if (error? ,r)
             (progn ,@body)
             ,r))))

(defun run (exp)
   (cond ((stringp exp) (funcall (str exp)))
         ((characterp exp) (funcall (chr exp)))
         ((numberp exp) (funcall (chr (code-char exp))))
         ((functionp exp) (funcall exp))
         (t (error "expected a parser, but found ~s instead" exp))))

(defun handle-result (r)
   (on-failure r
       (error (format-error r))))

(defun format-error (e)
	(case (error-type e)
		(expected 
			(format nil "expected ~a, but found ~a at position ~a"
			   (first (error-args e)) (second (error-args e)) (error-index e)))
		(custom (car (error-args e)))
		(otherwise (format nil "untyped error: ~a" e))))

(defmacro parse (src form)
   `(let ((*source* (make-source :value ,src))
          (*index* 0))
          (handle-result (run ,form))))

(declaim (inline test-remaining))
(defun test-remaining (n)
   (if (stringp (source-value *source*))
       (>= (length (source-value *source*)) (+ *index* n))
       t))

(defvar free-vars nil)
(defun handle-forms (forms)
   (let* ((free-vars nil)
          (new-forms (transform-binds forms)))
          (if free-vars 
              (list free-vars (butlast new-forms) (car (last new-forms)))
              (list new-forms))))

(defun generate-single-binding (var form)
  `(lambda () (setf ,var (run ,form)) ,var))

(defun generate-multiple-binding (vars form)
   (with-gensyms (parse-result)
     `(lambda ()
        (let ((,parse-result (run ,form))) 
          (if (listp ,parse-result) 
              (progn
                (when (not (eql (length ,parse-result) ,(length vars)))
                      (error "result of ~a is the wrong length for the variable list ~a" ',form '(,@vars)))
                ,@(iter (for x in vars)
                        (for y upfrom 0) 
                        (collect `(setf ,x (nth ,y ,parse-result)))))
              ,parse-result)))))

(defun transform-binds (forms)
   (iter (for exp in forms)
         (collect 
            (bind :on-failure exp
               ((multiple lhs) '<- rhs) exp
               (appendf free-vars lhs)
               (if (not (cdr lhs))
                   (generate-single-binding (car lhs) rhs)
                   (generate-multiple-binding lhs rhs))))))

(defun simple-pair (list val)
   (mapcar (lambda (x) (list x val)) list))

(defmacro defparser* (name args &body body)
   `(progn (defun ,name ,args (lambda () ,@body))
          ,(when (not args) 
                `(progn 
                  (defvar ,name (funcall #',name))
                  (setf ,name (funcall #',name))))))

(defun seq* (&rest functions)
   (lambda ()
      (iter (for f in functions)
            (for r = (run f))
            (collect (on-failure r (leave r))))))

(defmacro seq (&rest forms)
   (let ((r (handle-forms forms)))
      (if (eql (length r) 1) 
          (with-gensyms (a x y) 
           `(lambda ()
              (let ((,a (list ,@(first r)))) 
                (iter (for ,x in ,a)
                      (for ,y = (run ,x))
                      (on-failure ,y (leave ,y))
                      (finally (return ,y))))))
          (with-gensyms (a x y)
           `(lambda ()
              (let* ,(append (simple-pair (first r) nil) `((,a (list ,@(second r)))))
                (iter (for ,x in ,a)
                      (for ,y = (run ,x))
                      (on-failure ,y (leave ,y))
                      (finally (return ,(third r))))))))))

(defmacro defparser (name args &body body)
   `(progn (defun ,name ,args (seq ,@body))
          ,(when (not args) 
                `(progn 
                  (defvar ,name (funcall #',name))
                  (setf ,name (funcall #',name))))))

;; choice   ~ in the matching operation, returns the first form that consumes input
;; any      ~ matches zero or more of the next form
;; many     ~ matches one or more of the next form
;; one-of   ~ matches one character from the given string
;; none-of  ~ matches only if none of the chars in the string match
;; times    ~ parses x occurances of y
;; try      ~ returns a parsing result, no effect on the normal parser stack
;; optional ~ tries x, parses it on success, succeeds either way
;; seq      ~ matches each form sequentially, returns a list of forms or nil
;; str      ~ explicitly matches a string
;; sep      ~ seperates by repeated matching. mostly magic

(defparser* chr (char)
   (if (test-remaining 1) 
       (if (eql (get-character *index*) char)
           (pass char 1)
           (fail 'expected (list (string char) (get-character *index*))))
       (fail (string char) nil)))

(defparser* str (string)
   (iter (for char in-string string)
         (for parse-result = (funcall (chr char)))
         (on-failure parse-result (leave parse-result))
         (finally (return string))))

(defparser* one-of (s)
   (iter (for c in-string s)
         (on-success (funcall (try (chr c)))
            (leave (funcall (chr c))))
         (finally (return (fail s nil)))))

(defparser* none-of (s)
   (iter (for c in-string s)
         (on-success (funcall (try (chr c)))
            (leave (fail s nil)))
         (finally (return c))))

(defparser* range (a b)
   (if (> (char-code a) (char-code b))
       (range b a)
       (run
         (one-of 
           (kitten*
             (iter (for x from (char-code a) to (char-code b))
                   (collect (code-char x))))))))

(defparser* try (p)
   (let* ((*index* *index*))
    (run p)))

(defparser* any (p)
   (iter (for r = (run p))
         (until (error? r))
         (collect r into result)
         (finally (return (merge-characters result)))))

(defparser* many (p)
   (iter (for r = (run p))
         (until (error? r))
         (collect r into result)
         (finally (return (if result (merge-characters result) (fail 'many nil))))))

(defparser* choice (&rest parsers)
   (iter (for parser in parsers)
         (for parser-result = (run parser))
         (on-success parser-result (leave parser-result))
         (finally (return (fail 'choice nil)))))

(defparser* times (n parser)
   (iter (repeat n)
         (for parse-result = (run parser))
         (on-failure parse-result (leave parse-result))
         (collect parse-result into list)
         (finally (return (merge-characters list)))))

(defparser* optional (parser)
   (if (error? (run (try parser)))
       nil
       (run parser)))
