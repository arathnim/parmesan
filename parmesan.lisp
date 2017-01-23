;; (author *this-code*) => ("Dylan Ball" "Arathnim@gmail.com")

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

;; error type and functions

(declaim (inline error?))
(defstruct (parser-error (:conc-name error-) (:predicate error?))
   type index args)

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

;; 'interpreted' parsers, for dynamic calling at runtime
;; used as a fallback and for prototyping new parsers

(defparameter interpreted-parsers (make-hash-table :test #'equalp))
(defparameter compiled-parsers (make-hash-table :test #'equalp))

(defmacro define-parser (name ll &rest body)
  `(setf (gethash ',name interpreted-parsers)
         (lambda ,ll ,@body)))

(defmacro define-compiled-parser (name ll &rest body)
  `(setf (gethash ',name compiled-parsers)
         (lambda ,ll ,@body)))

(defun dynamic-run (exp)
  (aif (and (listp exp) (gethash (car exp) interpreted-parsers))
       (apply it (cdr exp))
       (error "unexpected form: ~a" exp)))

(defun compile-parser (exp)
  (aif (and (listp exp) (gethash (car exp) compiled-parsers))
       (apply it (cdr exp))
       (error "unexpected form: ~a" exp)))

(defmacro insert-compiled (exp)
   (compile-parser exp))

(defmacro parse (src form)
  `(let ((*source* (make-source :value ,src))
         (*index* 0))
         (dynamic-run ',form)))

(defmacro compiled-parse (src form)
  `(let ((*source* (make-source :value ,src))
         (*index* 0))
         (insert-compiled ,form)))

;; programmer options
;;  backtracking
;;  value-returning
;;  errors

(defvar *settings* '(:backtracking nil :value-returning t :errors t))

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

(declaim (inline test-remaining))
(defun test-remaining (n)
   (if (stringp (source-value *source*))
       (>= (length (source-value *source*)) (+ *index* n))
       t))

;; char

(define-parser chr (char)
   (if (test-remaining 1)
       (if (eql (get-character *index*) char)
           (pass char 1)
           (fail (string char) nil))
       (fail (string char) nil)))

(define-compiled-parser chr (char)
  (once-only (char)
   `(if (test-remaining 1)
        (if (eql (get-character *index*) ,char)
            (pass ,char 1)
            (fail (string ,char) nil))
        (fail (string ,char) nil))))

;; str

(define-parser str (string)
   (iter (for char in-string string)
         (for parse-result = (dynamic-run `(chr ,char)))
         (on-failure parse-result (leave parse-result))
         (finally (return string))))

(define-compiled-parser str (string)
   (once-only (string)
     `(iter (for char in-string ,string)
            (for parse-result = ,(compile-parser '(chr char)))
            (on-failure parse-result (leave parse-result))
            (finally (return ,string)))))

;; try

(define-parser try (p)
   (let* ((*index* *index*))
      (dynamic-run p)))

;; any

(define-parser any (p)
   (iter (for r = (dynamic-run p))
         (until (error? r))
         (collect r into result)
         (finally (return (merge-characters result)))))

;; many

(define-parser many (p)
   (iter (for r = (dynamic-run p))
         (until (error? r))
         (collect r into result)
         (finally (return (if result (merge-characters result) (fail 'many nil))))))

(define-parser choice (&rest parsers)
   (iter (for parser in parsers)
         (for parser-result = (dynamic-run parser))
         (on-success parser-result (leave parser-result))
         (finally (return (fail 'choice nil)))))

(define-parser times (n parser)
   (iter (repeat n)
         (for parse-result = (dynamic-run parser))
         (on-failure parse-result (leave parse-result))
         (collect parse-result into list)
         (finally (return (merge-characters list)))))

(define-parser optional (parser)
   (if (error? (dynamic-run (try parser)))
       nil
       (run parser)))
